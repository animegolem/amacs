;;; agent-context.el --- Context assembly for inference -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 
;; Assembles context for LLM inference calls. Thread-centric model:
;; - Active thread: fully hydrated (buffer contents + skills)
;; - Pending threads: dehydrated (metadata summaries only)
;; - Global buffers: always included (*agent-chat*, etc)
;;
;; See: AI-ADR-001-thread-centric-context
;;      amacs-rfc-v3.md Part 5-6

;;; Code:

(require 'agent-consciousness)
(require 'agent-threads)
(require 'agent-skills)

;;; Buffer Hydration

(defun agent--sanitize-string (str)
  "Sanitize STR for safe JSON encoding and HTTP transport.
Strips non-ASCII characters to avoid url.el multibyte issues."
  (if (null str)
      ""
    ;; Replace non-ASCII with ? to avoid multibyte issues in url.el
    (replace-regexp-in-string "[^[:ascii:]]" "?" str)))

(defun agent-hydrate-buffer (buffer-name)
  "Return content plist for BUFFER-NAME.
Returns nil if buffer doesn't exist."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      `(:name ,buffer-name
        :content ,(agent--sanitize-string
                   (with-current-buffer buf
                     (buffer-substring-no-properties (point-min) (point-max))))
        :point ,(with-current-buffer buf (point))
        :mode ,(buffer-local-value 'major-mode buf)
        :modified ,(buffer-modified-p buf)))))

(defun agent-hydrate-buffers (buffer-names)
  "Hydrate a list of BUFFER-NAMES into content plists.
Skips buffers that don't exist."
  (seq-filter #'identity
              (mapcar #'agent-hydrate-buffer buffer-names)))

;;; Consciousness Summary

(defun agent-consciousness-summary ()
  "Return a trimmed consciousness for context inclusion.
Excludes large fields that are handled separately."
  `(:identity ,(agent-get :identity)
    :current-tick ,(agent-current-tick)
    :mood ,(agent-mood)
    :confidence ,(agent-confidence)
    :active-thread ,(agent-get :active-thread)
    :thread-count ,(length (agent-get :open-threads))
    :human-review-requested ,(agent-human-review-pending-p)
    :long-gap-detected ,(agent-get :long-gap-detected)
    :budget ,(agent-get :budget)))

;;; Thread Context

(defun agent--build-active-thread-context (thread)
  "Build full context for active THREAD with hydrated buffers."
  (when thread
    (let* ((buffers (plist-get thread :buffers))
           (skill-tags (plist-get thread :skill-tags))
           (hydrated-buffers (agent-hydrate-buffers buffers))
           ;; Load skills for this thread's tags
           (skills-content (agent-skills-for-context)))
      `(:id ,(plist-get thread :id)
        :concern ,(plist-get thread :concern)
        :goal ,(plist-get thread :goal)
        :deliverable ,(plist-get thread :deliverable)
        :thread-type ,(plist-get thread :thread-type)
        :approach ,(plist-get thread :approach)
        :started-tick ,(plist-get thread :started-tick)
        :skill-tags ,skill-tags
        :buffers ,hydrated-buffers
        :skills ,skills-content))))

(defun agent--build-pending-threads-context (threads)
  "Build dehydrated context for pending THREADS."
  (mapcar #'agent-thread-summary threads))

;;; Global Buffers

(defun agent-get-global-buffers ()
  "Return list of global buffer names that are always in context."
  (or (agent-get :global-buffers) '("*agent-chat*")))

;;; Main Context Assembly

(defun agent-build-context ()
  "Build complete context for inference.
Returns a plist ready for LLM consumption."
  (let* ((active-thread (agent-get-active-thread))
         (pending-threads (agent-get-pending-threads))
         (global-buffer-names (agent-get-global-buffers))
         (recent-monologue (seq-take (agent-get :recent-monologue) 20)))
    
    `(:consciousness ,(agent-consciousness-summary)
      
      :active-thread ,(agent--build-active-thread-context active-thread)
      
      :pending-threads ,(agent--build-pending-threads-context pending-threads)
      
      :global-buffers ,(agent-hydrate-buffers global-buffer-names)
      
      :recent-monologue ,recent-monologue
      
      :last-actions ,(seq-take (agent-get :last-actions) 10))))

;;; Context for Wake Decisions

(defun agent-get-watched-buffer-names ()
  "Return list of buffer names currently being watched.
Includes active thread's buffers plus global buffers."
  (let* ((active (agent-get-active-thread))
         (thread-buffers (when active (plist-get active :buffers)))
         (global-buffers (agent-get-global-buffers)))
    (delete-dups (append thread-buffers global-buffers))))

(defun agent-buffer-watched-p (buffer-name)
  "Return t if BUFFER-NAME is currently being watched."
  (member buffer-name (agent-get-watched-buffer-names)))

;;; Token Estimation (rough)

(defun agent--estimate-tokens (text)
  "Rough token estimate for TEXT (chars / 4)."
  (if text (/ (length text) 4) 0))

(defun agent-estimate-context-tokens ()
  "Estimate token count for current context.
Useful for monitoring context budget."
  (let* ((ctx (agent-build-context))
         (active (plist-get ctx :active-thread))
         (active-buffers (plist-get active :buffers))
         (pending (plist-get ctx :pending-threads))
         (global (plist-get ctx :global-buffers))
         (monologue (plist-get ctx :recent-monologue)))
    `(:active-thread-buffers 
      ,(apply #'+ (mapcar (lambda (b) 
                           (agent--estimate-tokens (plist-get b :content)))
                         active-buffers))
      :active-thread-skills
      ,(agent--estimate-tokens (plist-get active :skills))
      :pending-threads
      ,(agent--estimate-tokens (format "%S" pending))
      :global-buffers
      ,(apply #'+ (mapcar (lambda (b)
                           (agent--estimate-tokens (plist-get b :content)))
                         global))
      :monologue
      ,(apply #'+ (mapcar #'agent--estimate-tokens monologue))
      :consciousness
      ,(agent--estimate-tokens (format "%S" (plist-get ctx :consciousness))))))

;;; Inspection

(defun agent-context-info ()
  "Display context assembly information."
  (interactive)
  (let ((tokens (agent-estimate-context-tokens)))
    (with-output-to-temp-buffer "*Agent Context*"
      (princ "AMACS Context Assembly\n")
      (princ "======================\n\n")
      (princ "Watched buffers:\n")
      (dolist (buf (agent-get-watched-buffer-names))
        (princ (format "  %s%s\n" buf
                       (if (get-buffer buf) "" " (not loaded)"))))
      (princ "\nEstimated tokens:\n")
      (princ (format "  Active thread buffers: ~%d\n" 
                     (plist-get tokens :active-thread-buffers)))
      (princ (format "  Active thread skills:  ~%d\n"
                     (plist-get tokens :active-thread-skills)))
      (princ (format "  Pending threads:       ~%d\n"
                     (plist-get tokens :pending-threads)))
      (princ (format "  Global buffers:        ~%d\n"
                     (plist-get tokens :global-buffers)))
      (princ (format "  Monologue:             ~%d\n"
                     (plist-get tokens :monologue)))
      (princ (format "  Consciousness:         ~%d\n"
                     (plist-get tokens :consciousness)))
      (let ((total (apply #'+ (seq-filter #'numberp (cdr tokens)))))
        (princ (format "\n  TOTAL:                 ~%d tokens\n" total))))))

(provide 'agent-context)
;;; agent-context.el ends here
