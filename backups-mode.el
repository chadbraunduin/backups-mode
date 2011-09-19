;;; backups-mode.el --- major mode for autosaving files and listing, viewing, and reverting Emacs generated backups
;; Copyright (C) 2011 by Chad Braun-Duin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Chad Braun-Duin <chadbraunduin@gmail.com>
;; Maintainer: Chad Braun-Duin <chadbraunduin@gmail.com>
;; Created: 27 Aug 2011
;; URL: <https://github.com/chadbraunduin/backups-mode>.
;; Keywords: backup, backups, autosaving, autosave, diff
;; Compatibility: Emacs 23+

;;; Commentary:
;;  The purpose of these commands and this new major mode is to loosely approximate the autosave-with-versions API
;;  recently instituted by Apple for Mac OS X Lion.

;;; Installation
;;  git clone git@github.com:chadbraunduin/backups-mode.git
;;  cd backups-mode
;;  # copy to your emacs load-path
;;  cp backups-mode.el ~/.emacs.d/
;;  # this assumes ~/.emacs.d/ is in your emacs load-path
;;  # add the following to .emacs
;;  (require 'backups-mode)

;;; Addtional Configuration
;;  ;; putting this in your .emacs will allow you to change version control settings. These are the default settings found in backups-mode.el.
;;  (setq backup-by-copying t
;;  delete-old-versions t
;;  kept-new-versions 6
;;  kept-old-versions 2
;;  version-control t)
;;  Documentation on these settings can be found here: <http://www.gnu.org/software/emacs/elisp/html_node/Numbered-Backups.html>

;;; My Personal Configuration
;;  As an example, here's the configuration from my .emacs file
;;  (defvar backup-directory "~/.emacs-backups/backups/")
;;  (defvar tramp-backup-directory "~/.emacs-backups/tramp-backups/")
;;  ;; backups-mode needs to be loaded after setting your backup directory variables
;;  (require 'backups-mode)
;;  (setq kept-new-versions 7
;;  kept-old-versions 3)

;;; Usage
;;  While editing a file-based buffer there are two new commands and some changes to note.
;;    Now, whenever you kill a buffer or kill emacs, all file-based buffers will be saved without prompting.
;;  New Commands while editing a file:
;;    save-version (\C-cv) will backup the previously saved version of the file.
;;    list-backups (\C-cb) will open a backups-mode buffer.
;;  The backups-mode buffer will list all backups Emacs has created for the file and will allow you these options:
;;    view-backup (<enter>) will open a backup file read-only.
;;    revert-backup (R) will backup the current file then replace the current file with the backup you've chosen.
;;    diff 2 files. (d + d) You can choose from the current file or any backup files and diff two of them.


;;; Code:

;;; global variables and .emacs configuation default values
(defvar backups-mode-hook nil)
(defvar last-modified-date-command-function 'nix-last-modified-date-command) ;; platform specific way of getting last modified date
(defvar unknown-last-modified-date "stat:") ;; platform specific output for unknown last modified date

(global-set-key "\C-cv" 'save-version)
(global-set-key "\C-cb" 'list-backups)
(global-set-key "\C-ck" 'kill-buffer-prompt)

;;; utilities
(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(or (fboundp 'diff-no-select)
    (defun diff-no-select (old new &optional switches no-async)
      (save-window-excursion (diff old new switches no-async))
      (get-buffer-create "*Diff*")))

(defun save-version ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer 16)) ;; archive a copy of the previous version)

(defun next-line-at-beginning ()
  (interactive)
  (next-line)
  (beginning-of-line))

(defun previous-line-at-beginning ()
  (interactive)
  (previous-line)
  (beginning-of-line))

;;; autosave configuration section
(defvar emacs-directory "~/.emacs.d/")

(defvar backup-directory (concat emacs-directory "backups/"))
(make-directory backup-directory t)
(setq backup-directory-alist `((".*" . ,backup-directory)))
(setq auto-save-list-file-prefix (concat backup-directory ".auto-saves-"))
(setq auto-save-file-name-transforms `((".*" ,backup-directory t)))

(defvar tramp-backup-directory (concat emacs-directory "tramp-backups/"))
(make-directory tramp-backup-directory t)
(setq tramp-backup-directory-alist `((".*" . ,tramp-backup-directory)))

;; this next line turns on emacs version control with backups
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defadvice kill-buffer (around kill-buffer)
  "Always save before killing a file buffer"
  (when (and (buffer-modified-p)
	     (buffer-file-name)
	     (file-exists-p (buffer-file-name)))
    (save-buffer))
  ad-do-it)
(ad-activate 'kill-buffer)

(defadvice save-buffers-kill-emacs (around save-buffers-kill-emacs)
  "Always save before killing emacs"
  (save-some-buffers t)
  ad-do-it)
(ad-activate 'save-buffers-kill-emacs)

(defun kill-buffer-prompt ()
  (interactive)
  (if (and (buffer-modified-p) (buffer-file-name) (file-exists-p (buffer-file-name)) (y-or-n-p "Save buffer?"))
      (save-buffer)
    (set-buffer-modified-p nil))
  (kill-buffer))

;;; list-backups and private helper methods
(defun get-filter-pattern (file-name)
  (concat (replace-regexp-in-string "\/" "!" file-name t t)
	  "\.~[0-9]*~*$"))

(defun filter-files (backup-directory buffer-file-name)
  (mapcar (lambda (backup-name) (concat backup-directory backup-name))
	  (filter (lambda (backup-name)
		    (string-match (get-filter-pattern buffer-file-name) backup-name))
		  (directory-files backup-directory))))

(defun make-version-number (file-name)
  (let ((try-version-index (string-match "~[0-9]+~$" file-name)))
    (when try-version-index
      (full-version-number file-name try-version-index))))

(defun full-version-number (file-name start &optional number-str)
  (let* ((number-str (or number-str ""))
	 (number (string-to-number number-str)))
    (if (< start (length file-name))
	(let ((current-char (substring file-name (+ 1 start) (+ 2 start))))
	  (cond ((equal current-char "0") (full-version-number file-name (+ 1 start) (concat number-str current-char)))
		((equal (string-to-number current-char) 0) number)
		(t (full-version-number file-name (+ 1 start) (concat number-str current-char)))))
      number)))

(defun make-last-modified-date (file-name)
  (let ((last-modified-date
	 (car
	  (split-string
	   (shell-command-to-string
	    (funcall last-modified-date-command-function file-name))))))
    (when (not (equal last-modified-date unknown-last-modified-date))
      last-modified-date)))

(defun nix-last-modified-date-command (file-name)
  (concat "stat -c %y " file-name))

(defun list-backups ()
  (interactive)
  (let ((old-buffer-name (buffer-name))
	(old-file-name (buffer-file-name)))
    (if old-file-name
	(progn
	  (switch-to-buffer (format "%s~backups" old-buffer-name))
	  (backups-mode) ;; switch to backups-mode
	  (erase-buffer)

	  (make-variable-buffer-local 'file-name)
	  (setq file-name old-file-name)

	  (make-variable-buffer-local 'buffer-name)
	  (setq buffer-name old-buffer-name)

	  (make-variable-buffer-local 'files)
	  (setq files (mapcar 'make-file (cons file-name (sort
							  (filter-files (get-backup-directory old-file-name) old-file-name)
							  'file-sort-p))))

	  (make-variable-buffer-local 'first-diff-index)
	  (setq first-diff-index nil)

	  (make-variable-buffer-local 'buffers-opened)
	  (setq buffers-opened '())
	  
	  ;; do pretty print here
	  (insert (format "backups for %s\n" file-name))
	  (insert
	   (apply 'concat (mapcar
			   (lambda (file)
			     (let* ((version (get-version file))
				    (version (if version (concat (number-to-string version) "\t") "current"))
				    (last-modified-date (or (get-last-modified-date file) (concat "unknown" "\t")))
				    (short-file-name (file-name-nondirectory (get-file-name file))))
			       (format "  %s\t%s\t%s\n" version last-modified-date short-file-name)))
			   files)))
	  ;; move the cursor to the top
	  (goto-char 1)
	  (next-line)
	  (beginning-of-line)
	  (set-buffer-modified-p nil))
      (princ "No backups for a non-file buffer"))))

;;; file structure methods
(defun make-file (file-name)
  (list
   (make-version-number file-name)
   (make-last-modified-date file-name)
   file-name))

(defun get-version (file)
  (nth 0 file))

(defun get-last-modified-date (file)
  (nth 1 file))

(defun get-file-name (file)
  (nth 2 file))

(defun file-sort-p (file-name1 file-name2)
  (let ((version1 (make-version-number file-name1))
	(version2 (make-version-number file-name2)))
    (> version1 version2)))

;;; backups mode private methods
(defun get-file-name-from-index (index)
  (get-file-name (nth index files)))

(defun get-index-number (line-number)
  (- line-number 2))

(defun get-line-number (index)
  (+ index 2))

(defun get-backup-directory (file-name)
  (if (tramp-tramp-file-p file-name)
      tramp-backup-directory
    backup-directory))

;;; backups mode map and methods
(defvar backups-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "<return>") 'view-backup)
    (define-key map "r" (lambda () (interactive) (princ "Use a capital R to revert")))
    (define-key map "R" 'revert-backup)
    (define-key map "d" 'diff-version)
    (define-key map "q" (lambda ()
			  (interactive)
			  (kill-buffer (buffer-name))
			  (mapc (lambda (buffer) (kill-buffer buffer)) buffers-opened))
      ) ;; quit buffer and cleanup all other buffers opened up in the process
    (define-key map [remap next-line] 'next-line-at-beginning)
    (define-key map [remap previous-line] 'previous-line-at-beginning)
    map)
  "Keymap for backups major mode")

(defun backups-mode ()
  "Major mode for viewing and reverting backup files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map backups-mode-map)
  (buffer-disable-undo)
  (setq header-line-format (format " <enter> to view (read-only), d + d to diff, R to revert, q to quit"))
  (setq major-mode 'backups-mode)
  (setq mode-name "Backups")
  (run-hooks 'backups-mode-hook))

(define-minor-mode view-backup-mode ()
  "Minor mode for viewing a single backup file"
  " Backup-file"
  '(("q" . (lambda () (interactive) (kill-buffer (buffer-name))))
    ("d" . (lambda ()
	     (interactive)
	     (let ((orig-file-name file-name))
	       (setq diff-buffer (diff-no-select file-name (buffer-file-name)))
	       (switch-to-buffer diff-buffer)
	       (setq file-name orig-file-name)
	       (push diff-buffer buffers-opened)))))
  :init-value nil)

(add-hook 'diff-mode-hook
	  (lambda ()
	    (setq buffer-read-only t)
	    (buffer-disable-undo)
	    (setq header-line-format (format " q to quit"))
	    (local-set-key "q" (lambda () (interactive) (kill-buffer (buffer-name))))))

(defun view-backup ()
  (interactive)
  (let ((index (get-index-number (line-number-at-pos)))
	(orig-file-name file-name))
    (cond ((zerop index)
	   (if (get-buffer buffer-name)
	       (switch-to-buffer buffer-name)
	     (find-file file-name)))
	  ((and (> index 0) (< index (length files)))
	   (setq ro-buffer (find-file-noselect (get-file-name-from-index index)))
	   (switch-to-buffer ro-buffer)
	   (view-backup-mode t)
	   (setq header-line-format (format " d to diff with current file, q to quit"))
	   (setq file-name orig-file-name)
	   (push ro-buffer buffers-opened))
	  (t (princ "No file on this line")))))

(defun revert-backup ()
  (interactive)
  (let ((index (get-index-number (line-number-at-pos))))
    (cond ((zerop index)
	   (princ "Cannot revert current buffer"))
	  ((and (> index 0) (< index (length files)))
	   (let* ((backup-file-name (get-file-name-from-index index))
		  (temp-backup-file-name (concat backup-file-name "#temp#")))
	     ;; using a temp file is necessary since saving the buffer may delete the backup file before it can be restored
	     (copy-file backup-file-name temp-backup-file-name)
	     (kill-buffer) ;; kill the backups-mode buffer
	     (switch-to-buffer buffer-name)
	     (save-buffer) ;; first, save the buffer. This is so the current changes become a saved version
	     (save-version) ;; save a version of the current buffer
	     (kill-buffer) ;; kill the original buffer
	     (copy-file temp-backup-file-name file-name t) ;; move the temp file to become the current file
	     (delete-file temp-backup-file-name)
	     (find-file file-name)))
	  (t (princ "No file on this line")))))

(defun diff-version ()
  (interactive)
  (let* ((line-number (line-number-at-pos))
	 (index (get-index-number line-number)))
    (if (and (>= index 0) (< index (length files)))
	(progn
	  (cond ((eq first-diff-index index)
		 (beginning-of-line)
		 (delete-char 1)
		 (insert " ")
		 (setq first-diff-index nil)
		 (beginning-of-line))
		(first-diff-index
		 (goto-line (get-line-number first-diff-index))
		 (delete-char 1)
		 (insert " ")
		 (goto-line line-number)
		 (progn
		   (when (and
			  (zerop first-diff-index)
			  (get-buffer buffer-name)
			  (buffer-modified-p (get-buffer buffer-name)))
		     (let ((backups-mode-buffer-name (buffer-name)))
		       (switch-to-buffer buffer-name)
		       (save-buffer)
		       (switch-to-buffer backups-mode-buffer-name)))
		   (let ((first-file-name (get-file-name-from-index first-diff-index))
			 (second-file-name (get-file-name-from-index index)))
		     (setq first-diff-index nil)
		     (setq diff-buffer (diff-no-select first-file-name second-file-name))
		     (set-buffer-modified-p nil)
		     (switch-to-buffer diff-buffer)
		     (push diff-buffer buffers-opened))))
		(t
		 (setq first-diff-index index)
		 (beginning-of-line)
		 (insert "d")
		 (delete-char 1)
		 (next-line)
		 (beginning-of-line)))
	  (set-buffer-modified-p nil))
      (princ "No file on this line"))))

(provide 'backups-mode)

;;; backups-mode.el ends here