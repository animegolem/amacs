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
(require 'agent-scratchpad)

;;; Buffer Hydration

(defun agent--sanitize-string (str)
  "Sanitize STR for safe JSON encoding and HTTP transport.
Strips non-ASCII characters to avoid url.el multibyte issues."
  (if (null str)
      ""
    ;; Replace non-ASCII with ? to avoid multibyte issues in url.el
    (replace-regexp-in-string "[^[:ascii:]]" "?" str)))

(defun agent-hydrate-buffer (buffer-name)
  "Return content alist for BUFFER-NAME.
Returns nil if buffer doesn't exist."
  (let ((buf (get-buffer buffer-name)))
    (when buf
      `((name . ,buffer-name)
        (content . ,(agent--sanitize-string
                     (with-current-buffer buf
                       (buffer-substring-no-properties (point-min) (point-max)))))
        (point . ,(with-current-buffer buf (point)))
        (mode . ,(buffer-local-value 'major-mode buf))
        (modified . ,(buffer-modified-p buf))))))

(defun agent-hydrate-buffers (buffer-names)
  "Hydrate a list of BUFFER-NAMES into content alists.
Skips buffers that don't exist."
  (seq-filter #'identity
              (mapcar #'agent-hydrate-buffer buffer-names)))

;;; Consciousness for Context

(defun agent-consciousness-for-context ()
  "Return consciousness state for context inclusion.
Includes all fields except those shown in dedicated sections
\(recent-monologue, last-actions, open-threads details).
Agent needs full self-visibility for effective self-management."
  `((identity . ,(agent-get 'identity))
    ;; Temporal
    (current-tick . ,(agent-current-tick))
    (current-time . ,(agent-get 'current-time))
    (last-inference-time . ,(agent-get 'last-inference-time))
    (long-gap-detected . ,(agent-get 'long-gap-detected))
    ;; Affective
    (mood . ,(agent-mood))
    (confidence . ,(agent-confidence))
    ;; Threads (summary - details in active-thread section)
    (active-thread . ,(agent-get 'active-thread))
    (thread-budget . ,(agent-get 'thread-budget))
    (thread-count . ,(length (agent-get 'open-threads)))
    (completed-thread-count . ,(length (agent-get 'completed-threads)))
    ;; Context controls
    (chat-context-depth . ,(agent-get 'chat-context-depth))
    (monologue-context-depth . ,(agent-get 'monologue-context-depth))
    (global-buffers . ,(agent-get 'global-buffers))
    (focus . ,(agent-get 'focus))
    ;; Memory pointers
    (last-commit . ,(agent-get 'last-commit))
    ;; Skills
    (active-skills . ,(agent-get 'active-skills))
    ;; Human interaction
    (human-review-requested . ,(agent-human-review-pending-p))
    (chat-pending . ,(not (null (agent-get 'chat-pending))))
    ;; Eval tracking
    (has-eval-result . ,(not (null (agent-get 'last-eval-result))))
    ;; Budget
    (budget . ,(agent-get 'budget))))

;; Backward compatibility alias
(defalias 'agent-consciousness-summary 'agent-consciousness-for-context)

;;; Thread Context

(defun agent--build-active-thread-context (thread)
  "Build full context for active THREAD with hydrated buffers. Returns alist."
  (when thread
    (let* ((buffers (alist-get 'buffers thread))
           (skill-tags (alist-get 'skill-tags thread))
           (hydrated-buffers (agent-hydrate-buffers buffers))
           ;; Load skills for this thread's tags
           (skills-content (agent-skills-for-context)))
      `((id . ,(alist-get 'id thread))
        (concern . ,(alist-get 'concern thread))
        (goal . ,(alist-get 'goal thread))
        (deliverable . ,(alist-get 'deliverable thread))
        (thread-type . ,(alist-get 'thread-type thread))
        (approach . ,(alist-get 'approach thread))
        (started-tick . ,(alist-get 'started-tick thread))
        (skill-tags . ,skill-tags)
        (buffers . ,hydrated-buffers)
        (skills . ,skills-content)))))

(defun agent--build-pending-threads-context (threads)
  "Build dehydrated context for pending THREADS."
  (mapcar #'agent-thread-summary threads))

;;; Global Buffers

(defun agent-get-global-buffers ()
  "Return list of global buffer names that are always in context.
Discovers buffers by mode (amacs-chat-mode, agent-scratchpad-mode)
rather than hardcoded names."
  (or (agent-get-mode-buffers)
      (agent-get 'global-buffers)
      '()))

;;; Main Context Assembly

(defun agent-build-context ()
  "Build complete context for inference.
Returns an alist ready for LLM consumption.
Respects agent-controlled depth settings from consciousness."
  (let* ((active-thread (agent-get-active-thread))
         (pending-threads (agent-get-pending-threads))
         (global-buffer-names (agent-get-global-buffers))
         (monologue-depth (or (agent-get 'monologue-context-depth) 20))
         (recent-monologue (seq-take (agent-get 'recent-monologue) monologue-depth)))

    `((consciousness . ,(agent-consciousness-for-context))
      (active-thread . ,(agent--build-active-thread-context active-thread))
      (pending-threads . ,(agent--build-pending-threads-context pending-threads))
      (global-buffers . ,(agent-hydrate-buffers global-buffer-names))
      (recent-monologue . ,recent-monologue)
      (last-actions . ,(seq-take (agent-get 'last-actions) 10)))))

;;; Context for Wake Decisions

(defun agent-get-watched-buffer-names ()
  "Return list of buffer names currently being watched.
Includes active thread's buffers plus global buffers."
  (let* ((active (agent-get-active-thread))
         (thread-buffers (when active (alist-get 'buffers active)))
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
         (active (alist-get 'active-thread ctx))
         (active-buffers (alist-get 'buffers active))
         (pending (alist-get 'pending-threads ctx))
         (global (alist-get 'global-buffers ctx))
         (monologue (alist-get 'recent-monologue ctx)))
    `((active-thread-buffers
       . ,(apply #'+ (mapcar (lambda (b)
                               (agent--estimate-tokens (alist-get 'content b)))
                             (or active-buffers '()))))
      (active-thread-skills
       . ,(agent--estimate-tokens (alist-get 'skills active)))
      (pending-threads
       . ,(agent--estimate-tokens (format "%S" pending)))
      (global-buffers
       . ,(apply #'+ (mapcar (lambda (b)
                               (agent--estimate-tokens (alist-get 'content b)))
                             (or global '()))))
      (monologue
       . ,(apply #'+ (mapcar #'agent--estimate-tokens (or monologue '()))))
      (consciousness
       . ,(agent--estimate-tokens (format "%S" (alist-get 'consciousness ctx)))))))

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
                     (alist-get 'active-thread-buffers tokens)))
      (princ (format "  Active thread skills:  ~%d\n"
                     (alist-get 'active-thread-skills tokens)))
      (princ (format "  Pending threads:       ~%d\n"
                     (alist-get 'pending-threads tokens)))
      (princ (format "  Global buffers:        ~%d\n"
                     (alist-get 'global-buffers tokens)))
      (princ (format "  Monologue:             ~%d\n"
                     (alist-get 'monologue tokens)))
      (princ (format "  Consciousness:         ~%d\n"
                     (alist-get 'consciousness tokens)))
      (let ((total (apply #'+ (mapcar #'cdr tokens))))
        (princ (format "\n  TOTAL:                 ~%d tokens\n" total))))))

(provide 'agent-context)
;;; agent-context.el ends here
