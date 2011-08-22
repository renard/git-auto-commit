;;; git-auto-commit.el --- Git auto commit directory.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2011-06-28
;; Last changed: 2011-08-22 18:42:57
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Configure repositories to watch:
;;
;;     (eval-after-load 'git-auto-commit
;;       '(progn
;;          (setq gac-dir-set
;;                '("~/path/to/git"
;;                  "~/path/to/other/important/files"))))
;;
;;  Initialize `gac-commit-file' in `after-save-hook'.
;;
;;  If you are not using autoloads, you should require git-auto-commit:
;;    (require 'git-auto-commit nil t)



(eval-when-compile (require 'files))
(eval-when-compile (require 'timer))
(eval-when-compile (require 'simple))

(defgroup git-auto-commit nil
  "Options concerning git auto commit managment."
  :group 'tools)

(defcustom gac-dir-set '()
  "Set of git repositories to auto-commit using
  `gac-commit-file'."
  :type '(repeat directory)
  :group 'git-auto-commit)

(defcustom gac-schedule-push-delay 10
  "Idle time delay before pushing the commit."
  :type 'integer
  :group 'git-auto-commit)

(defcustom gac-default-cmd-git-add
  "git add \"%s\""
  "Default command to add a file to git index. This string is
passed to `format' with the saved filename in parameter."
  :type 'string
  :group 'git-auto-commit)

(defcustom gac-default-cmd-git-commit
  "git commit -m \"Auto commit %s.\""
  "Default command to commit file. This string is passed to
`format' with the commited filename in parameter."
  :type 'string
  :group 'git-auto-commit)

(defcustom gac-default-cmd-git-push
  "git push && /bin/true"
  "Default command to push commit."
  :type 'string
  :group 'git-auto-commit)


(defun gac-match-filep (f)
  "Test if file F is in a subdirectory of `gac-dir-set'."
  (car
   (remove 'nil
	   (mapcar
	    (lambda(x)
	      (setq x (abbreviate-file-name
		       (file-name-as-directory x)))
	      (when
		  (and
		   (file-directory-p (concat x ".git"))
		   (ignore-errors
		     (string-match x (substring f 0 (length x)))))
		x))
	    gac-dir-set))))


(defun gac-schedule-push (dn)
  "Schedule a push if one is not already scheduled for the given dir."
  (message (format "Scheduling to push %s" dn))
  (if (null (member dn gac-dir-set))
      (run-with-idle-timer
       gac-schedule-push-delay nil
       (lambda (dn)
	 (let ((default-directory dn))
	   (message "Pushing git repository from  %s" dn)
	   (shell-command gac-default-cmd-git-push)))
       dn)))

;;;###autoload
(defun gac-commit-file ()
  "Commit file visited in current buffer."
  (interactive)
  (let* ((fn (abbreviate-file-name (buffer-file-name)))
	 (dn (gac-match-filep fn))
	 rn)
    (when dn
      (message "git adding %s" fn)
      (setq  rn (file-relative-name fn dn))
      (let ((default-directory dn))
	(shell-command (format gac-default-cmd-git-add rn))
	(shell-command (format 
			gac-default-cmd-git-commit rn)))
      (gac-schedule-push dn))))

(provide 'git-auto-commit)
