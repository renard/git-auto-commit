;;; git-auto-commit.el --- Git auto commit directory.

;; Copyright © 2011 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2011-06-28
;; Last changed: 2011-09-14 23:24:38
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;  Configure repositories to watch:
;;
;;     (eval-after-load 'git-auto-commit
;;       '(progn
;;          (setq gac-dir-set
;;                '(("~/path/to/git")
;;                  ("~/path/to/other/important/files")))))
;;
;;  Initialize `gac-commit-file' in `after-save-hook'.
;;
;;  If you are not using autoloads, you should require git-auto-commit:
;;    (require 'git-auto-commit nil t)



(eval-when-compile (require 'files))
(eval-when-compile (require 'timer))
(eval-when-compile (require 'simple))
(eval-when-compile (require 'cl))

(defgroup git-auto-commit nil
  "Options concerning git auto commit managment."
  :group 'tools)

(defcustom gac-dir-set '()
  "Set of git repositories to auto-commit using
`gac-commit-file'. Each item is a repository description
containing:

  (\"/path/to/repository\" CONFIGURATION)

Where CONFIGURATION is PLIST elements:

:schedule-push-delay

  Override `gac-default-schedule-push-delay'.

:cmd-git-add

  Override `gac-default-cmd-git-add'.


:cmd-git-commit

  Override `gac-default-cmd-git-commit'.

:cmd-git-push

  Override `gac-default-cmd-git-push'.
"
  :type 'list
  :group 'git-auto-commit)

(defcustom gac-default-schedule-push-delay 10
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

(defcustom gac-on-untracked-file 'ask
  "How to act when current file is untracked."
  :type '(choice (const :tag "Ask if file should be added" ask)
                 (const :tag "Add without asking" t)
                 (const :tag "Skip file" nil))
  :group 'git-auto-commit)

(defun gac-get-repositories ()
  "Extract directory list from `gac-dir-set'."
  (loop for x in gac-dir-set
        collect (car x)))

(defun gac-get-repo-config (repo)
  "Extract repository configuration from `gac-dir-set'."
  (let* ((repository (assoc repo gac-dir-set))
         (conf (copy-alist (cdr repository))))
    (when repository
      ;; put default values to repository if not defined.
      (loop for x in '(schedule-push-delay cmd-git-add cmd-git-commit cmd-git-push)
            do (setq conf
                     (plist-put conf (intern (format ":%s" x))
                                (or (plist-get conf (intern (format ":%s" x)))
                                    (eval (intern (format "gac-default-%s" x)))))))
      conf)))

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
            (gac-get-repositories)))))

(defun gac-schedule-push (dn conf)
  "Schedule a push if one is not already scheduled for the given dir."
  (message (format "Scheduling to push %s" dn))
  (when (member dn (gac-get-repositories))
    (run-with-idle-timer
     (plist-get conf :schedule-push-delay) nil
     (lambda (dn conf)
       (let* ((default-directory (file-name-as-directory dn)))
         (message "Pushing git repository from  %s" dn)
         (shell-command (plist-get conf :cmd-git-push))))
     dn conf)))

(defun gac-get-file-ignored-or-untracked (git-root fn)
  "Check if file FN is ignored or untracked and return matching symbol or nil otherwise."
  (let ((default-directory git-root))
    (cond
     ((not (string-blank-p (shell-command-to-string (format "git ls-files -i -o --exclude-standard %s" fn)))) :ignored)
     ((not (vc-git-registered fn)) :untracked)
     (t nil))))

;;;###autoload
(defun gac-commit-file ()
  "Commit file visited in current buffer."
  (interactive)
  (let* ((fn (abbreviate-file-name (buffer-file-name)))
         (dn (gac-match-filep fn))
         (rn (file-relative-name fn dn))
         (status (gac-get-file-ignored-or-untracked dn rn)))
    (when (and dn
               (not (eq status :ignored))
               (if (eq status :untracked)
                   (cond
                    ((eq gac-on-untracked-file 'ask) (y-or-n-p (format "Add file \"%s\" to the index?" rn)))
                    (t gac-on-untracked-file))
                 t))
      (message "git adding %s" rn)
      (let* ((default-directory dn)
             (dirname (directory-file-name dn))
             (conf (gac-get-repo-config dirname)))
        (shell-command (format (plist-get conf :cmd-git-add) rn))
        (shell-command (format (plist-get conf :cmd-git-commit) rn))
        (gac-schedule-push dirname conf)))))

(provide 'git-auto-commit)
