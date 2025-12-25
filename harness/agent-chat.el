;;; agent-chat.el --- Human-agent chat interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Org-mode based chat interface for human-agent communication.
;;
;; Structure:
;;   Human types in #+begin_prompt / #+end_prompt blocks.
;;   Agent response transforms block into:
;;     * Tick N
;;     ** Human Prompt (original block preserved)
;;     ** Agent Response
;;
;; See: AI-IMP-027-chat-interface-redesign

;;; Code:

(require 'org)
(require 'org-element)
(require 'agent-consciousness)
(require 'agent-tick)

;; Forward declaration to avoid circular require
(declare-function agent-think "agent-inference")

;;; Minor Mode

(defvar amacs-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'amacs-chat-send)
    map)
  "Keymap for `amacs-chat-mode'.")

;;;###autoload
(define-minor-mode amacs-chat-mode
  "Minor mode for AMACS chat buffers.
\\{amacs-chat-mode-map}"
  :lighter " AMACS"
  :keymap amacs-chat-mode-map
  (when amacs-chat-mode
    ;; Add prompt template when mode is enabled
    (agent-chat--setup-tempo)))

;;; Tempo Template for Prompt Blocks

(defun agent-chat--setup-tempo ()
  "Setup org-tempo template for prompt blocks."
  (when (bound-and-true-p org-structure-template-alist)
    (unless (assoc "h" org-structure-template-alist)
      (add-to-list 'org-structure-template-alist
                   '("h" . "prompt")))))

(defun agent-chat-insert-prompt-block ()
  "Insert a prompt block at point."
  (interactive)
  (insert "#+begin_prompt\n\n#+end_prompt")
  (forward-line -1))

;;; Prompt Block Detection

(defun agent-chat-find-pending-prompt (&optional buffer)
  "Find pending prompt block in BUFFER (default: current buffer).
Returns alist with :start, :end, :content keys, or nil if none found.
A pending prompt is one not yet wrapped in a Tick heading."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      ;; Find prompt blocks that are NOT inside a Tick heading
      (let ((found nil))
        (while (and (not found)
                    (re-search-forward "^#\\+begin_prompt" nil t))
          (let ((block-start (match-beginning 0)))
            ;; Check if this block is inside a * Tick heading
            (save-excursion
              (goto-char block-start)
              (let ((in-tick-heading
                     (and (re-search-backward "^\\* " nil t)
                          (looking-at "^\\* Tick [0-9]+"))))
                (unless in-tick-heading
                  ;; Found a pending prompt - extract content
                  (goto-char block-start)
                  (when (re-search-forward "^#\\+end_prompt" nil t)
                    (let ((block-end (match-end 0))
                          (content-start (save-excursion
                                          (goto-char block-start)
                                          (forward-line 1)
                                          (point)))
                          (content-end (match-beginning 0)))
                      (setq found
                            (list (cons 'start block-start)
                                  (cons 'end block-end)
                                  (cons 'content
                                        (string-trim
                                         (buffer-substring-no-properties
                                          content-start content-end))))))))))))
        found))))

(defun agent-chat-has-pending-prompt-p (&optional buffer)
  "Return t if BUFFER has a pending prompt block."
  (not (null (agent-chat-find-pending-prompt buffer))))

;;; Chat Commands

(defun amacs-chat-send ()
  "Queue chat for agent attention and trigger think.
Sets chat-pending in consciousness and invokes agent-think."
  (interactive)
  (unless (agent-chat-has-pending-prompt-p)
    (user-error "No pending prompt block found. Use <h TAB to insert one"))
  (agent-set 'chat-pending
             (list (cons 'buffer (buffer-name))
                   (cons 'file (buffer-file-name))
                   (cons 'queued-at (current-time))))
  (message "Chat queued for agent attention")
  ;; In Phase 2, immediately trigger think
  (when (fboundp 'agent-think)
    (agent-think)))

;;; Response Transformation

(defun agent-chat-respond (response-text &optional think-text)
  "Transform pending prompt into tick structure and add response.
RESPONSE-TEXT is the agent's response to the human.
THINK-TEXT is optional reasoning trace (will be collapsed)."
  (when-let* ((chat-info (agent-get 'chat-pending))
              (buf (get-buffer (alist-get 'buffer chat-info))))
    (with-current-buffer buf
      (when-let* ((prompt-info (agent-chat-find-pending-prompt)))
        (let* ((block-start (alist-get 'start prompt-info))
               (block-end (alist-get 'end prompt-info))
               (tick (agent-current-tick))
               (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
          ;; Go to start of block
          (goto-char block-start)
          ;; Insert tick heading before the block
          (insert (format "* Tick %d\n" tick))
          (insert ":PROPERTIES:\n")
          (insert (format ":COMPLETED: %s\n" timestamp))
          (insert ":END:\n\n")
          (insert "** Human Prompt\n")
          ;; Block content is now after our insertions
          ;; Move past the prompt block
          (goto-char (+ block-end
                        (- (point) block-start)))
          (insert "\n\n** Agent Response\n")
          (when think-text
            (insert "*** Think\n" think-text "\n\n"))
          (insert response-text "\n")
          ;; Save buffer if it has a file
          (when buffer-file-name
            (save-buffer))))))
  ;; Clear pending
  (agent-chat-clear-pending))

;;; Chat Reading

(defun agent-chat-read-exchanges (n &optional buffer)
  "Read last N exchanges from chat BUFFER.
Returns list of alists with human and agent content."
  (let ((target-buffer (or buffer
                           (when-let* ((info (agent-get 'chat-pending)))
                             (get-buffer (alist-get 'buffer info)))
                           (current-buffer))))
    (with-current-buffer target-buffer
      (let ((exchanges '())
            (all-headlines '()))
        ;; First collect all headlines with their levels
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hl)
            (push hl all-headlines)))
        (setq all-headlines (nreverse all-headlines))
        ;; Process Tick headings and their children
        (let ((i 0))
          (while (< i (length all-headlines))
            (let* ((hl (nth i all-headlines))
                   (title (org-element-property :raw-value hl))
                   (level (org-element-property :level hl)))
              (when (and (= level 1)
                         (string-match "^Tick \\([0-9]+\\)$" title))
                (let ((tick (string-to-number (match-string 1 title)))
                      (human-content nil)
                      (agent-content nil))
                  ;; Look at following headlines until we hit another level-1
                  (cl-incf i)
                  (while (and (< i (length all-headlines))
                              (> (org-element-property :level (nth i all-headlines)) 1))
                    (let* ((sub (nth i all-headlines))
                           (sub-title (org-element-property :raw-value sub))
                           (sub-level (org-element-property :level sub)))
                      (when (= sub-level 2)
                        (cond
                         ((string= sub-title "Human Prompt")
                          (setq human-content
                                (agent-chat--extract-prompt-content sub)))
                         ((string= sub-title "Agent Response")
                          (setq agent-content
                                (agent-chat--extract-response-content sub))))))
                    (cl-incf i))
                  (push (list (cons 'tick tick)
                              (cons 'human human-content)
                              (cons 'agent agent-content))
                        exchanges)
                  ;; Back up one since the outer loop will increment
                  (cl-decf i))))
            (cl-incf i)))
        ;; Return last N exchanges (exchanges is in reverse order)
        (seq-take (nreverse exchanges) n)))))

(defun agent-chat--extract-prompt-content (headline)
  "Extract prompt block content from HEADLINE."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline)))
    (when (and begin end)
      (save-excursion
        (goto-char begin)
        (if (re-search-forward "#\\+begin_prompt" end t)
            (progn
              (forward-line 1)
              (let ((content-start (point)))
                (when (re-search-forward "#\\+end_prompt" end t)
                  (string-trim
                   (buffer-substring-no-properties
                    content-start (line-beginning-position))))))
          ;; Fallback: just get the heading content up to subheading
          (let ((subhead (save-excursion
                          (re-search-forward "^\\*\\*\\* " end t))))
            (string-trim
             (buffer-substring-no-properties
              begin (or subhead end)))))))))

(defun agent-chat--extract-response-content (headline)
  "Extract response content from HEADLINE, excluding Think subheading.
Response content comes after any *** Think section."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline)))
    (when (and begin end)
      (save-excursion
        (goto-char begin)
        ;; Check if Think subheading exists
        (if (re-search-forward "^\\*\\*\\* Think" end t)
            ;; Skip past Think content - find blank line separator
            (progn
              (forward-line 1)  ; Move past *** Think line
              ;; Skip Think content until blank line
              (when (re-search-forward "^$" end t)
                (forward-line 1)  ; Move past blank line
                (string-trim
                 (buffer-substring-no-properties (point) end))))
          ;; No Think section - get all content
          (string-trim
           (buffer-substring-no-properties begin end)))))))

(defun agent-chat-last-human-input (&optional buffer)
  "Return the most recent human input from chat BUFFER.
Checks for pending prompt first, then last exchange."
  (let ((buf (or buffer
                 (when-let* ((info (agent-get 'chat-pending)))
                   (get-buffer (alist-get 'buffer info)))
                 (current-buffer))))
    (or
     ;; First check for pending prompt
     (when-let* ((pending (agent-chat-find-pending-prompt buf)))
       (alist-get 'content pending))
     ;; Otherwise get last exchange
     (when-let* ((exchanges (agent-chat-read-exchanges 1 buf)))
       (alist-get 'human (car exchanges))))))

;;; Chat Buffer Management

(defun agent-chat-clear-pending ()
  "Clear chat pending flag after agent has responded."
  (agent-set 'chat-pending nil))

(defun agent-chat-pending-p ()
  "Return t if there is pending chat to handle."
  (not (null (agent-get 'chat-pending))))

(defun agent-chat-buffer-name ()
  "Return the name of the pending chat buffer, or nil."
  (when-let* ((info (agent-get 'chat-pending)))
    (alist-get 'buffer info)))

(defun agent-create-chat-buffer (&optional name)
  "Create a new chat buffer with NAME (default: *amacs-chat*).
Sets up org-mode and amacs-chat-mode with proper structure."
  (let* ((buf-name (or name "*amacs-chat*"))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (amacs-chat-mode 1)
      (when (= (buffer-size) 0)
        (insert "#+TITLE: AMACS Chat\n")
        (insert "#+STARTUP: showall\n\n")
        (insert "Type your message in a prompt block and press C-c C-c to send.\n")
        (insert "Use <h TAB to insert a prompt block.\n\n")
        (insert "#+begin_prompt\n\n#+end_prompt\n")))
    buf))

(provide 'agent-chat)
;;; agent-chat.el ends here
