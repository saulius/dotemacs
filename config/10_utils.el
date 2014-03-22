;; save file trimming all whitespace
(defun save-buffer-no-whitespace ()
  "Writes buffer with trimmed whitespace"
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer)
  )

(defun kill-this-buffer-and-window ()
  "kills buffer and window (split)"
  (interactive)
  (kill-this-buffer)
  (delete-window)
  )

(defun my-enter ()
  "Inserts a newline character then indents the new line just
like the previous line"
  (interactive)
  (newline)
  (indent-relative-maybe))

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))

;; from http://tuxicity.se/emacs/elisp/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

