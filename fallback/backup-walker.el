;;; backup-walker.el --- quickly traverse all backups of a file

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Chad Braun-Duin
;; Description: quickly traverse all backups of a file

;; Created: Wed Sep  7 19:38:05 2011 (+0800)
;; Version: 2.0
;; Last-Updated: Web Oct 12 22:54 2011 (+0800)
;;           By: Chad Braun-Duin
;; URL: https://github.com/chadbraunduin/backups-mode
;; Keywords: backup
;; Compatibility: Emacs 23+

;;; Installation:

;;
;; add to ~/.emacs.el
;;
;;  (require 'backup-walker)
;;
;; This installation is unnecessary if you've already put this version of
;; backup-walker.el and bm-utilities.el in your emacs load-path and have
;; required and started backups-mode

;;; Commentary (from lewang):

;; I never delete backups.  They are versioned in their own directory, happy
;; and safe.  My fingers skip to C-x C-s whenever I pause to think about
;; anything.  Even when I'm working with VCS, I save far more often than I
;; commit.
;;
;; This package helps me traverse those backups if I'm looking for something.
;;
;; The typical workflow is:
;;
;;   1) I'm in a buffer and realize I need to check some backups.
;;
;;        C-cw
;;
;;   2) I press <p> to go backwards in history until I see something
;;      interesting.  Then I press <enter> to bring it up.  OOPs this isn't
;;      it, I go back to the backup-walker window and find the right file.
;;
;;   3) I get what I need from the backup, go back to backup-walker, and press
;;      <q> and kill all open backups.
;;
;;   4) the end.
;;
;; Additionally, note that all the diff-mode facilities are available in the
;; `backup-walker' buffer.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(provide 'backup-walker)
(load "bm-utilities.el")

(eval-when-compile (require 'cl))
(eval-and-compile (require 'diff))

(defun backups-mode-p ()
  (fboundp 'backups-mode-start))

(defvar backup-walker-ro-map (make-sparse-keymap))
(define-key backup-walker-ro-map [(n)] 'backup-walker-next)
(define-key backup-walker-ro-map [(p)] 'backup-walker-previous)
(define-key backup-walker-ro-map [(q)] 'backup-walker-quit)
(define-key backup-walker-ro-map [(return)] 'backup-walker-show-file-in-other-window)
(when (backups-mode-p)
  (define-key backup-walker-ro-map [(b)] (lambda ()
					   (interactive)
					   (list-backups-from-file
					    (cdr (assq :original-file backup-walker-data-alist))
					    :data backup-walker-data-alist))))

(define-derived-mode backup-walker-mode diff-mode "Diff backup-walker"
  "major mode for traversing versioned backups.  Use
  `backup-walker-start' as entry point."
  (run-hooks 'view-mode-hook)           ; diff-mode sets up this hook to
                                        ; remove its read-only overrides.
  (add-to-list 'minor-mode-overriding-map-alist `(buffer-read-only . ,backup-walker-ro-map)))


(defun backup-walker-minor-mode (&optional arg)
  "purposefully made non-interactive, because this mode should only be used by code"
  (setq arg (cond  ((or (null arg)
                        (eq arg 'toggle))
                    (if backup-walker-minor-mode
                        nil
                      t))
                   ((> arg 0)
                    t)
                   ((<= arg 0)
                    nil)))
  (setq backup-walker-minor-mode arg)
  (force-mode-line-update)
  (if backup-walker-minor-mode
      (let ((index (cdr (assq :index backup-walker-data-alist)))
            (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist))))
	(setq header-line-format (backup-walker-get-key-help-common index suffixes (concat "viewing " (propertize (int-to-string
														    (backup-walker-get-version
														     (nth index suffixes)))
														   'face 'font-lock-keyword-face)
											   ", ")))
        (add-to-list 'minor-mode-overriding-map-alist `(buffer-read-only . ,backup-walker-ro-map))
	(when (backups-mode-p)
	  (define-key backup-walker-ro-map "r" (lambda () (interactive) (princ bm-revert-message)))
	  (define-key backup-walker-ro-map "R" (lambda () (interactive)
						 (bm-revert-backup-from-file (cdr (assq :original-file backup-walker-data-alist))
									     (buffer-file-name)
									     backup-walker-data-alist)))))
    (setq header-line-format nil)
    (delq `(buffer-read-only . ,backup-walker-ro-map) minor-mode-overriding-map-alist))
  backup-walker-minor-mode)

(add-minor-mode 'backup-walker-minor-mode " walker" nil nil nil)

(defvar backup-walker-data-alist nil "")
(make-variable-buffer-local 'backup-walker-data-alist)

(defsubst backup-walker-get-version (fn &optional start)
  "return version number given backup"
  (if start
      (string-to-int
       (substring fn
                  (string-match "[[:digit:]]+" fn start)
                  (match-end 0)))
    (backup-walker-get-version fn (length (file-name-sans-versions fn)))))

(defsubst backup-walker-get-key-help-common (index suffixes current-file-string)
  (concat
   (if (eq index 0)
       (if (eq major-mode 'backup-walker-mode)
	   (concat (propertize "current" 'face 'font-lock-keyword-face)
		", ")
	 "")
     (concat (propertize "<n> " 'face 'italic)
	     (propertize (int-to-string (backup-walker-get-version (nth (1- index) suffixes)))
                         'face 'font-lock-keyword-face)
             ", "))
   current-file-string
   (if (nth (1+ index) suffixes)
       (concat (propertize "<p> " 'face 'italic)
	       (propertize (int-to-string
                            (backup-walker-get-version (nth (1+ index) suffixes)))
                           'face 'font-lock-keyword-face)
               ", ")
     "")
   (when backup-walker-minor-mode
     "<R> revert, ")
   (propertize "<q>" 'face 'italic)
   " quit"))

;; TODO: We can actually compute the correct new point by diffing and
;;       interpreting results.  So far, it seems overkill.
(defsubst backup-walker-move (index-cons index suffixes new-index)
  "internal function used by backup-walker-{next,previous}"
  (cond
   ((eq major-mode 'backup-walker-mode)
    (setcdr index-cons new-index)
    (backup-walker-refresh))
   (backup-walker-minor-mode
    (let* ((prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
           (file-name (concat prefix (nth new-index suffixes)))
           (alist (copy-alist backup-walker-data-alist))
           (buf (find-file-noselect file-name)))
      (setcdr (assq :index alist) new-index)
      (with-current-buffer buf
        (setq backup-walker-data-alist alist)
        (backup-walker-minor-mode 1))
      (switch-to-buffer buf)
      (bm-rename-buffer file-name backup-walker-data-alist)))))

(defun backup-walker-get-sorted-backups (filename)
  "Return version sorted list of backups of the form:
  (prefix (list of suffixes))"
  ;; `make-backup-file-name' will get us the right directory for
  ;; ordinary or numeric backups.  It might create a directory for
  ;; backups as a side-effect, according to `backup-directory-alist'.
  (let* ((filename (file-name-sans-versions
                    (make-backup-file-name (expand-file-name filename))))
         (file (file-name-nondirectory filename))
         (dir  (file-name-directory    filename))
         (comp (file-name-all-completions file dir))
         (prefix-len (length file)))
    (cons filename (mapcar
                    (lambda (f)
                      (substring (cdr f) prefix-len))
                    (sort
		     (mapcar (lambda (f)
			       (cons (backup-walker-get-version f prefix-len)
				     f))
			     comp)
		     (lambda (f1 f2)
		       (not (< (car f1) (car f2)))))))))


(defun backup-walker-refresh ()
  (let* ((index (cdr (assq :index backup-walker-data-alist)))
         (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
         (prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
         (right-file (concat prefix (nth index suffixes)))
         (right-version (format "%i" (backup-walker-get-version right-file)))
         diff-buf left-file left-version)
	;; (debug)
    (if (eq index 0)
        (setq left-file (cdr (assq :original-file backup-walker-data-alist))
              left-version "orig")
      (setq left-file (concat prefix (nth (1- index) suffixes))
            left-version (format "%i" (backup-walker-get-version left-file))))
    (setq diff-buf (diff-no-select left-file right-file nil 'noasync))
	;; (debug)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (insert-buffer-substring diff-buf)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (force-mode-line-update)
    (setq header-line-format
          (concat (when (backups-mode-p)
		    "<b> backups-mode, ")
		  (backup-walker-get-key-help-common index
						     suffixes
						     (concat (propertize "<return>" 'face 'italic)
							     " opens "
							     (propertize (propertize (int-to-string (backup-walker-get-version right-file))
										     'face 'font-lock-keyword-face))
							     ", "))))
    (kill-buffer diff-buf)))

;;;###autoload
(defun* backup-walker-start (original-file &key (index 0) data)
  "start walking with the latest backup
with universal arg, ask for a file-name."
  (interactive (list (if (and current-prefix-arg (listp current-prefix-arg))
                         (read-file-name
                          "Original file: "
                          nil
                          buffer-file-name
                          t
                          (file-name-nondirectory buffer-file-name))
                       (or buffer-file-name
                           (error "buffer has no file name")))))
  (unless (and version-control
               (not (eq version-control 'never)))
    (error "version-control must be enabled for backup-walker to function."))
  (let ((first-config (current-window-configuration))
		(backups (backup-walker-get-sorted-backups original-file))
		buf)
    
    (if (null (cdr backups))
        (error "no backups found.")
      
      (unless (assq :original-file data)
		(push `(:original-file . ,original-file) data))
      (push `(:backup-prefix . ,(car backups)) data)
      (push `(:backup-suffix-list . ,(cdr backups)) data)
      (push `(:index . ,(+ 0)) data)
      (unless (assq :first-config data)
		(push `(:first-config . ,first-config) data))

      (setq buf (get-buffer-create (format "*Walking: %s*" (buffer-name (get-file-buffer original-file)))))
      (with-current-buffer buf
		(push `(:walking-buffer . ,buf) data)
        (backup-walker-mode))
      (bm-switch-to-window buf 'backups-major-mode-p)
      (buffer-disable-undo)
      (setq second-config (current-window-configuration))
      (if (assq :second-config data)
		  (setcdr (assq :second-config data) second-config)
		(push `(:second-config . ,second-config) data))
      (setq backup-walker-data-alist data)
      (backup-walker-refresh)
      (when (> index 0)
		(backup-walker-previous index))
      buf)))


(defun backup-walker-next (arg)
  "move to a more recent backup
with ARG, move ARG times"
  (interactive "p")
  (cond ((< arg 0)
         (backup-walker-previous (- arg)))
        ((> arg 0)
         (let* ((index-cons (assq :index backup-walker-data-alist))
                (index (cdr index-cons))
                (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
                (new-index (- index arg)))
           (if (< new-index 0)
               (signal 'args-out-of-range (list (format "not enough newer backups, max is %i" index)))
             (backup-walker-move index-cons index suffixes new-index))))))

(defun backup-walker-previous (arg)
  "move to a less recent backup
with ARG move ARG times"
  (interactive "p")
  (cond ((< arg 0)
         (backup-walker-next (- arg)))
        ((> arg 0)
         (let* ((index-cons (assq :index backup-walker-data-alist))
                (index (cdr index-cons))
                (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
                (max-movement (- (1- (length suffixes)) index))
                (new-index (+ index arg)))
           (if (> arg max-movement)
               (signal 'args-out-of-range (list (format "not enough older backups, max is %i" max-movement)))
             (backup-walker-move index-cons index suffixes new-index))))))

(defun backup-walker-show-file-in-other-window ()
  "open the current backup "
  (interactive)
  (unless (eq major-mode 'backup-walker-mode)
    (error "this is not a backup walker buffer."))
  (let* ((index (cdr (assq :index backup-walker-data-alist)))
         (suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
         (prefix (cdr (assq :backup-prefix backup-walker-data-alist)))
         (file-name (concat prefix (nth index suffixes)))
         (walking-buf (current-buffer))
         (alist (copy-alist backup-walker-data-alist))
         (buf (find-file-noselect file-name)))
    (with-current-buffer buf
      (setq backup-walker-data-alist alist)
      (backup-walker-minor-mode 1))
    (bm-switch-to-window buf 'backups-minor-mode-p)
    (bm-rename-buffer file-name backup-walker-data-alist)))

(defun backup-walker-blame (line)
  "find out where a certain line came into existance
show the backup *JUST BEFORE* this line was born, since that is
usually more interesting."
  (interactive (list (read-string "line: " nil 'backup-walker-blame)))
  (cond
   (backup-walker-minor-mode
    (let* ((my-index (cdr (assq :index backup-walker-data-alist)))
           (walking-buf (cdr (assq :walking-buffer backup-walker-data-alist))))
      (with-current-buffer walking-buf
        (setcdr (assq :index backup-walker-data-alist) my-index)
        (backup-walker-refresh))
      (switch-to-buffer walking-buf)
      (backup-walker-blame line)))
   ((eq major-mode 'backup-walker-mode)
    (let* ((index-cons (assq :index backup-walker-data-alist))
           (old-index (cdr index-cons))
           (is-unified (member "-u" diff-switches))
           (search-str (format "-%s" line))
           found)
      (condition-case err
          (while (not found)
            (let ((suffixes (cdr (assq :backup-suffix-list backup-walker-data-alist)))
                  (index (cdr (assq :index backup-walker-data-alist))))
              (when (not is-unified)
                (diff-context->unified (point-min) (point-max)))
              (message "searching %s" (nth index suffixes))
              (if (search-forward search-str nil t)
                  (setq found t)
                (backup-walker-previous 1))))
        ('args-out-of-range
         (setcdr index-cons old-index)
         (backup-walker-refresh)
         (message "input was not found in backup history")))))
   (t
    (error "not sure what you want me to do."))))

(defun backup-walker-quit (arg)
  "quit backup-walker session.
Also, kill all associated backup buffers."
  (interactive "P")
  (cond (backup-walker-minor-mode
	 (bm-kill-popup-buffer backup-walker-data-alist))
	((eq major-mode 'backup-walker-mode)
	 (bm-kill-all-buffers backup-walker-data-alist))
	(t
	 (error "I don't know how to quit you."))))


;; also require chadbraunduin's backups-mode if it exists
(require 'backups-mode nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backup-walker.el ends here
