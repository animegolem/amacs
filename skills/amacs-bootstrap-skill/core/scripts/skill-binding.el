;;; skill-binding.el --- Bind skills to contexts -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for associating skills with major modes, buffers, and projects.
;; Skills load automatically when their bound context is active.

;;; Code:

(defvar agent-mode-skills '()
  "Alist mapping major modes to skill names.
When a buffer's major-mode matches, the associated skill loads.")

(defvar agent-buffer-skills '()
  "Alist mapping buffer name patterns (regexps) to skill names.
When a buffer name matches the pattern, the associated skill loads.")

(defvar agent-project-skills '()
  "Alist mapping project root paths to skill names.
When working in a project, the associated skill loads.")

(defvar agent-skills-directory "~/.agent/skills/"
  "Directory containing skill folders.")

;;; Binding Functions

(defun bind-skill-to-mode (skill-name mode)
  "Register SKILL-NAME to load when entering MODE.
MODE should be a symbol like 'rust-mode or 'python-mode.

Example:
  (bind-skill-to-mode \"rust-mode\" 'rust-mode)"
  (let ((existing (assoc mode agent-mode-skills)))
    (if existing
        (setcdr existing skill-name)
      (push (cons mode skill-name) agent-mode-skills)))
  (message "Bound skill '%s' to mode '%s'" skill-name mode))

(defun bind-skill-to-buffer (skill-name buffer-pattern)
  "Register SKILL-NAME to load for buffers matching BUFFER-PATTERN.
BUFFER-PATTERN is a regexp matched against buffer names.

Example:
  (bind-skill-to-buffer \"project-amacs\" \"amacs.*\\\\.el\")"
  (let ((existing (assoc buffer-pattern agent-buffer-skills)))
    (if existing
        (setcdr existing skill-name)
      (push (cons buffer-pattern skill-name) agent-buffer-skills)))
  (message "Bound skill '%s' to buffer pattern '%s'" skill-name buffer-pattern))

(defun bind-skill-to-project (skill-name project-root)
  "Register SKILL-NAME to load when working in PROJECT-ROOT.
PROJECT-ROOT should be an absolute path to the project directory.

Example:
  (bind-skill-to-project \"my-project\" \"/home/user/projects/my-project/\")"
  (let* ((normalized-root (file-name-as-directory (expand-file-name project-root)))
         (existing (assoc normalized-root agent-project-skills)))
    (if existing
        (setcdr existing skill-name)
      (push (cons normalized-root skill-name) agent-project-skills)))
  (message "Bound skill '%s' to project '%s'" skill-name project-root))

;;; Unbinding Functions

(defun unbind-skill-from-mode (mode)
  "Remove any skill binding for MODE."
  (setq agent-mode-skills (assoc-delete-all mode agent-mode-skills)))

(defun unbind-skill-from-buffer (buffer-pattern)
  "Remove any skill binding for BUFFER-PATTERN."
  (setq agent-buffer-skills (assoc-delete-all buffer-pattern agent-buffer-skills)))

(defun unbind-skill-from-project (project-root)
  "Remove any skill binding for PROJECT-ROOT."
  (let ((normalized-root (file-name-as-directory (expand-file-name project-root))))
    (setq agent-project-skills (assoc-delete-all normalized-root agent-project-skills))))

;;; Retrieval Functions

(defun agent-get-skill-path (skill-name)
  "Return the path to SKILL-NAME's SKILL.md file."
  (expand-file-name 
   (concat skill-name "/SKILL.md") 
   agent-skills-directory))

(defun agent-skill-exists-p (skill-name)
  "Return t if SKILL-NAME exists (has a SKILL.md file)."
  (file-exists-p (agent-get-skill-path skill-name)))

(defun agent-get-relevant-skills (&optional buffer)
  "Return list of skill names relevant to BUFFER (default: current buffer).
Checks mode bindings, buffer name patterns, and project bindings."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (let ((mode-skill (alist-get major-mode agent-mode-skills))
            (buffer-skills 
             (mapcar #'cdr
                     (seq-filter 
                      (lambda (pair)
                        (string-match-p (car pair) (buffer-name)))
                      agent-buffer-skills)))
            (project-skills
             (when (buffer-file-name)
               (let ((file-path (expand-file-name (buffer-file-name))))
                 (mapcar #'cdr
                         (seq-filter
                          (lambda (pair)
                            (string-prefix-p (car pair) file-path))
                          agent-project-skills))))))
        (delete-dups
         (seq-filter #'identity
                     (append (when mode-skill (list mode-skill))
                             buffer-skills
                             project-skills)))))))

(defun agent-load-skill (skill-name)
  "Load SKILL-NAME by reading its SKILL.md content.
Returns the content as a string, or nil if skill doesn't exist."
  (let ((skill-path (agent-get-skill-path skill-name)))
    (if (file-exists-p skill-path)
        (with-temp-buffer
          (insert-file-contents skill-path)
          (buffer-string))
      (message "Warning: Skill '%s' not found at %s" skill-name skill-path)
      nil)))

(defun agent-load-skill-reference (skill-name reference-file)
  "Load a reference file from SKILL-NAME's references directory.
REFERENCE-FILE is the filename (e.g., \"patterns.md\").
Returns the content as a string, or nil if not found."
  (let ((ref-path (expand-file-name
                   (concat skill-name "/references/" reference-file)
                   agent-skills-directory)))
    (if (file-exists-p ref-path)
        (with-temp-buffer
          (insert-file-contents ref-path)
          (buffer-string))
      (message "Warning: Reference '%s' not found in skill '%s'" 
               reference-file skill-name)
      nil)))

;;; Listing Functions

(defun agent-list-all-bindings ()
  "Return a summary of all skill bindings as a string."
  (concat
   "Mode bindings:\n"
   (if agent-mode-skills
       (mapconcat (lambda (pair)
                    (format "  %s -> %s" (car pair) (cdr pair)))
                  agent-mode-skills "\n")
     "  (none)")
   "\n\nBuffer pattern bindings:\n"
   (if agent-buffer-skills
       (mapconcat (lambda (pair)
                    (format "  \"%s\" -> %s" (car pair) (cdr pair)))
                  agent-buffer-skills "\n")
     "  (none)")
   "\n\nProject bindings:\n"
   (if agent-project-skills
       (mapconcat (lambda (pair)
                    (format "  %s -> %s" (car pair) (cdr pair)))
                  agent-project-skills "\n")
     "  (none)")))

(defun agent-list-available-skills ()
  "Return list of all available skill names (directories with SKILL.md)."
  (when (file-directory-p agent-skills-directory)
    (seq-filter
     (lambda (name)
       (and (file-directory-p (expand-file-name name agent-skills-directory))
            (not (string-prefix-p "." name))
            (file-exists-p (agent-get-skill-path name))))
     (directory-files agent-skills-directory))))

;;; Persistence

(defvar agent-skill-bindings-file "~/.agent/skill-bindings.el"
  "File where skill bindings are persisted.")

(defun agent-save-skill-bindings ()
  "Save current skill bindings to disk."
  (with-temp-file agent-skill-bindings-file
    (insert ";;; Saved skill bindings\n")
    (insert (format "(setq agent-mode-skills '%S)\n" agent-mode-skills))
    (insert (format "(setq agent-buffer-skills '%S)\n" agent-buffer-skills))
    (insert (format "(setq agent-project-skills '%S)\n" agent-project-skills))))

(defun agent-load-skill-bindings ()
  "Load skill bindings from disk."
  (when (file-exists-p agent-skill-bindings-file)
    (load agent-skill-bindings-file t t)))

(provide 'skill-binding)
;;; skill-binding.el ends here
