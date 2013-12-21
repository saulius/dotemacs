(el-get 'sync '(org-pomodoro))

(require 'remember)
(require 'org-pomodoro)

(setq org-directory (concat "~" "/.emacs.d/org/"))
(setq org-global-agenda-file-name "agenda.org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-log-done t)
;; save clock history accross emacs sessions
(setq org-clock-persist t)
(setq org-clock-in-resume t)
(org-clock-persistence-insinuate)
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-agenda-files (list (concat org-directory "/todo.org")))

;; as seen in https://github.com/cofi/dotfiles/blob/b9c7027e759b21d6dd5c0401692c470d38387350/emacs.d/config/cofi-org.el
(defun visit-org-agenda-files (fname)
  "Visit agenda files.
Note: This assumes all files are in the org-directory."
  (interactive (list (completing-read "Visit file: "
                                    (mapcar 'file-name-nondirectory (org-agenda-files))
                                    nil t)))
  (find-file (concat org-directory fname)))

(defun open-org-global-todo ()
  "Opens global agenda"
    (interactive)
    (visit-org-agenda-files org-global-agenda-file-name))
