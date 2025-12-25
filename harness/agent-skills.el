;;; agent-skills.el --- Skill system integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 
;; Integrates the skill system into the AMACS harness.
;; Skills are directories with SKILL.md files that extend agent capabilities.
;; 
;; Skills bind to contexts (modes, buffer patterns, projects) and load
;; automatically when relevant. The core skill always loads.
;;
;; See: amacs-rfc-v3.md Part 8

;;; Code:

(require 'agent-consciousness)

;;; Variables

(defvar agent-skills-directory "~/.agent/skills/"
  "Directory containing skill folders.")

(defvar agent-mode-skills '()
  "Alist mapping major modes to skill names.")

(defvar agent-buffer-skills '()
  "Alist mapping buffer name patterns (regexps) to skill names.")

(defvar agent-project-skills '()
  "Alist mapping project root paths to skill names.")

(defvar agent-skill-bindings-file "~/.agent/skill-bindings.el"
  "File where skill bindings are persisted.")

(defvar agent-bootstrap-skills-source
  (let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    ;; harness/ is at same level as skills/
    (expand-file-name "../skills/amacs-bootstrap-skill/" dir))
  "Source location for all bootstrap skills (in repo).
Contains subdirectories: core/, chat/, etc.")

;;; Binding Functions

(defun bind-skill-to-mode (skill-name mode)
  "Register SKILL-NAME to load when entering MODE.
MODE should be a symbol like rust-mode or python-mode."
  (let ((existing (assoc mode agent-mode-skills)))
    (if existing
        (setcdr existing skill-name)
      (push (cons mode skill-name) agent-mode-skills)))
  (message "Bound skill '%s' to mode '%s'" skill-name mode))

(defun bind-skill-to-buffer (skill-name buffer-pattern)
  "Register SKILL-NAME to load for buffers matching BUFFER-PATTERN.
BUFFER-PATTERN is a regexp matched against buffer names."
  (let ((existing (assoc buffer-pattern agent-buffer-skills)))
    (if existing
        (setcdr existing skill-name)
      (push (cons buffer-pattern skill-name) agent-buffer-skills)))
  (message "Bound skill '%s' to buffer pattern '%s'" skill-name buffer-pattern))

(defun bind-skill-to-project (skill-name project-root)
  "Register SKILL-NAME to load when working in PROJECT-ROOT."
  (let* ((normalized-root (file-name-as-directory (expand-file-name project-root)))
         (existing (assoc normalized-root agent-project-skills)))
    (if existing
        (setcdr existing skill-name)
      (push (cons normalized-root skill-name) agent-project-skills)))
  (message "Bound skill '%s' to project '%s'" skill-name project-root))

;;; Skill Path Functions

(defun agent-get-skill-path (skill-name)
  "Return the path to SKILL-NAME's SKILL.md file."
  (expand-file-name 
   (concat skill-name "/SKILL.md") 
   agent-skills-directory))

(defun agent-skill-exists-p (skill-name)
  "Return t if SKILL-NAME exists (has a SKILL.md file)."
  (file-exists-p (agent-get-skill-path skill-name)))

(defun agent-list-available-skills ()
  "Return list of available skill names (excludes core).
Core skill is always in system prompt, not bindable to threads."
  (let ((dir (expand-file-name agent-skills-directory)))
    (when (file-directory-p dir)
      (seq-filter
       (lambda (name)
         (and (not (equal name "core"))  ; Exclude core
              (file-directory-p (expand-file-name name dir))
              (not (string-prefix-p "." name))
              (file-exists-p (agent-get-skill-path name))))
       (directory-files dir)))))

;;; Skill Loading

(defun agent-load-skill (skill-name)
  "Load SKILL-NAME by reading its SKILL.md content.
Returns the content as a string, or nil if skill doesn't exist."
  (let ((skill-path (agent-get-skill-path skill-name)))
    (if (file-exists-p skill-path)
        (prog1
            (with-temp-buffer
              (insert-file-contents skill-path)
              (buffer-string))
          ;; Track usage
          (agent-record-skill-use skill-name))
      (message "Warning: Skill '%s' not found at %s" skill-name skill-path)
      nil)))

(defun agent-load-skill-reference (skill-name reference-file)
  "Load a reference file from SKILL-NAME's references directory."
  (let ((ref-path (expand-file-name
                   (concat skill-name "/references/" reference-file)
                   agent-skills-directory)))
    (if (file-exists-p ref-path)
        (with-temp-buffer
          (insert-file-contents ref-path)
          (buffer-string))
      nil)))

;;; Thread-Based Skill Binding (IMP-023)

(declare-function agent-get-thread "agent-threads")
(declare-function agent--update-thread "agent-threads")

(defun agent-thread-bound-skills (&optional thread-id)
  "Return list of skills bound to THREAD-ID (default: active thread)."
  (let ((thread (agent-get-thread (or thread-id (agent-get 'active-thread)))))
    (alist-get 'bound-skills thread)))

(defun agent-bind-skill-to-thread (skill-name &optional thread-id)
  "Bind SKILL-NAME to THREAD-ID (default: active thread).
The skill's SKILL.md will load in context while this thread is active."
  (let* ((tid (or thread-id (agent-get 'active-thread)))
         (thread (agent-get-thread tid))
         (current-skills (or (alist-get 'bound-skills thread) '())))
    (unless tid
      (error "No active thread to bind skill to"))
    (unless (member skill-name (agent-list-available-skills))
      (error "Skill '%s' not found in ~/.agent/skills/" skill-name))
    (unless (member skill-name current-skills)
      (agent--update-thread tid
        `((bound-skills . ,(cons skill-name current-skills)))))
    (format "Bound skill '%s' to thread '%s'" skill-name tid)))

(defun agent-unbind-skill-from-thread (skill-name &optional thread-id)
  "Unbind SKILL-NAME from THREAD-ID (default: active thread)."
  (let* ((tid (or thread-id (agent-get 'active-thread)))
         (thread (agent-get-thread tid))
         (current-skills (alist-get 'bound-skills thread)))
    (unless tid
      (error "No active thread to unbind skill from"))
    (agent--update-thread tid
      `((bound-skills . ,(remove skill-name current-skills))))
    (format "Unbound skill '%s' from thread '%s'" skill-name tid)))

(defun agent--load-skill-content (skill-name)
  "Load SKILL.md content for SKILL-NAME. Returns string or nil."
  (let ((skill-path (agent-get-skill-path skill-name)))
    (when (file-exists-p skill-path)
      (with-temp-buffer
        (insert-file-contents skill-path)
        (buffer-string)))))

(defun agent--load-thread-skills ()
  "Load all skills bound to active thread. Returns formatted string or nil."
  (let* ((skills (agent-thread-bound-skills))
         (skill-contents
          (seq-filter #'cdr
            (mapcar (lambda (skill)
                      (cons skill (agent--load-skill-content skill)))
                    skills))))
    (when skill-contents
      (format "## Thread Skills\n\n%s"
              (mapconcat
               (lambda (pair)
                 (format "### %s\n\n%s" (car pair) (cdr pair)))
               skill-contents
               "\n\n---\n\n")))))

;;; Context-Based Skill Resolution

(defun agent-get-relevant-skills (&optional buffer)
  "Return list of skill names relevant to BUFFER (default: current buffer).
Always includes core. Checks mode, buffer pattern, and project bindings."
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
        ;; Core always included, then context-specific skills
        (delete-dups
         (cons "core"
               (seq-filter #'identity
                           (append (when mode-skill (list mode-skill))
                                   buffer-skills
                                   project-skills))))))))

(defun agent-load-relevant-skills (&optional buffer)
  "Load and return content of all relevant skills for BUFFER.
Returns an alist of (skill-name . content) pairs."
  (let ((skill-names (agent-get-relevant-skills buffer)))
    (seq-filter 
     #'cdr  ; Remove skills that failed to load
     (mapcar (lambda (name)
               (cons name (agent-load-skill name)))
             skill-names))))

(defun agent-skills-for-context (&optional buffer)
  "Return concatenated skill content for BUFFER, formatted for context.
This is what gets included in inference context."
  (let ((skills (agent-load-relevant-skills buffer)))
    (mapconcat
     (lambda (pair)
       (format "<skill name=\"%s\">\n%s\n</skill>" 
               (car pair) (cdr pair)))
     skills
     "\n\n")))

;;; Skill Use Tracking

(defun agent-record-skill-use (skill-name)
  "Record that SKILL-NAME was used, updating active-skills."
  (let* ((active (or (agent-get 'active-skills) '()))
         (existing (assoc skill-name active)))
    (if existing
        ;; Increment use count - the cdr is an alist now
        (setf (alist-get 'use-count (cdr existing))
              (1+ (or (alist-get 'use-count (cdr existing)) 0)))
      ;; Add new entry with alist value
      (push (cons skill-name
                  `((loaded-tick . ,(agent-current-tick))
                    (use-count . 1)))
            active))
    (agent-set 'active-skills active)))

;;; Bindings Persistence

(defun agent-save-skill-bindings ()
  "Save current skill bindings to disk."
  (let ((file (expand-file-name agent-skill-bindings-file)))
    (with-temp-file file
      (insert ";;; -*- lexical-binding: t; -*-\n")
      (insert ";;; Saved skill bindings\n")
      (insert (format "(setq agent-mode-skills '%S)\n" agent-mode-skills))
      (insert (format "(setq agent-buffer-skills '%S)\n" agent-buffer-skills))
      (insert (format "(setq agent-project-skills '%S)\n" agent-project-skills)))))

(defun agent-load-skill-bindings ()
  "Load skill bindings from disk."
  (let ((file (expand-file-name agent-skill-bindings-file)))
    (when (file-exists-p file)
      (load file t t))))

;;; Bootstrap Skill Installation

(defun agent--copy-skill-directory (skill-name source-base target-base)
  "Copy a single skill SKILL-NAME from SOURCE-BASE to TARGET-BASE.
Copies SKILL.md and references/ directory."
  (let ((source-dir (expand-file-name skill-name source-base))
        (target-dir (expand-file-name skill-name target-base)))
    (when (and (file-directory-p source-dir)
               (file-exists-p (expand-file-name "SKILL.md" source-dir)))
      ;; Only copy if target doesn't exist
      (unless (file-exists-p (expand-file-name "SKILL.md" target-dir))
        (message "Installing %s skill from %s" skill-name source-dir)
        ;; Create target directory
        (make-directory target-dir t)
        ;; Copy SKILL.md
        (copy-file (expand-file-name "SKILL.md" source-dir)
                   (expand-file-name "SKILL.md" target-dir)
                   t)
        ;; Copy references directory if it exists
        (let ((ref-source (expand-file-name "references" source-dir))
              (ref-target (expand-file-name "references" target-dir)))
          (when (file-directory-p ref-source)
            (make-directory ref-target t)
            (dolist (file (directory-files ref-source t "^[^.]"))
              (when (file-regular-p file)
                (copy-file file
                           (expand-file-name (file-name-nondirectory file) ref-target)
                           t)))))
        (message "%s skill installed to %s" skill-name target-dir)))))

(defun agent-ensure-bootstrap-skills ()
  "Ensure all bootstrap skills exist in ~/.agent/skills/.
Copies core, chat, and any other skills from repo if not present."
  (let ((source-base agent-bootstrap-skills-source)
        (target-base (expand-file-name agent-skills-directory)))
    (when (file-directory-p source-base)
      ;; Iterate over all subdirectories in bootstrap source
      (dolist (entry (directory-files source-base nil "^[^.]"))
        (when (file-directory-p (expand-file-name entry source-base))
          (agent--copy-skill-directory entry source-base target-base))))))

;; Backward compatibility alias
(defalias 'agent-ensure-core-skill 'agent-ensure-bootstrap-skills)

;;; Initialization

(defun agent-init-skills ()
  "Initialize the skill system.
Ensures directories exist, installs all bootstrap skills, loads bindings."
  ;; Ensure skills directory exists
  (let ((dir (expand-file-name agent-skills-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  ;; Install all bootstrap skills if missing
  (agent-ensure-bootstrap-skills)
  ;; Load saved bindings
  (agent-load-skill-bindings)
  ;; Record core skill as active
  (agent-record-skill-use "core")
  (message "Skill system initialized. Available: %s"
           (agent-list-available-skills)))

;;; Inspection

(defun agent-skill-info ()
  "Display skill system information."
  (interactive)
  (with-output-to-temp-buffer "*Agent Skills*"
    (princ "AMACS Skill System\n")
    (princ "==================\n\n")
    (princ (format "Skills directory: %s\n"
                   (expand-file-name agent-skills-directory)))
    (princ (format "Available skills: %s\n\n"
                   (or (agent-list-available-skills) "(none)")))
    (princ "Mode Bindings:\n")
    (if agent-mode-skills
        (dolist (pair agent-mode-skills)
          (princ (format "  %s -> %s\n" (car pair) (cdr pair))))
      (princ "  (none)\n"))
    (princ "\nBuffer Pattern Bindings:\n")
    (if agent-buffer-skills
        (dolist (pair agent-buffer-skills)
          (princ (format "  \"%s\" -> %s\n" (car pair) (cdr pair))))
      (princ "  (none)\n"))
    (princ "\nProject Bindings:\n")
    (if agent-project-skills
        (dolist (pair agent-project-skills)
          (princ (format "  %s -> %s\n" (car pair) (cdr pair))))
      (princ "  (none)\n"))
    (princ "\nActive Skills (from consciousness):\n")
    (let ((active (agent-get 'active-skills)))
      (if active
          (dolist (entry active)
            (princ (format "  %s: loaded tick %d, used %d times\n"
                           (car entry)
                           (alist-get 'loaded-tick (cdr entry))
                           (alist-get 'use-count (cdr entry)))))
        (princ "  (none)\n")))))

(provide 'agent-skills)
;;; agent-skills.el ends here
