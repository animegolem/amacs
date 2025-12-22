;;; agent-chat.el --- Human-agent chat interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Org-mode based chat interface for human-agent communication.
;; The agent reads and writes to the chat buffer like any other buffer.
;;
;; Structure:
;;   * Human Input     - human's turn
;;   * Agent Response  - agent's turn
;;     ** Think        - reasoning (collapsed by default)
;;     ** Output       - response to human
;;
;; See: AI-IMP-022-chat-interface

;;; Code:

(require 'org)
(require 'org-element)
(require 'agent-consciousness)

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
  :keymap amacs-chat-mode-map)

;;; Chat Commands

(defun amacs-chat-send ()
  "Queue chat for agent attention and trigger think.
Sets :chat-pending in consciousness and invokes agent-think."
  (interactive)
  (agent-set :chat-pending
             (list :buffer (buffer-name)
                   :file (buffer-file-name)
                   :queued-at (current-time)))
  (message "Chat queued for agent attention")
  ;; In Phase 2, immediately trigger think
  (when (fboundp 'agent-think)
    (agent-think)))

;;; Org Parsing Helpers

(defun agent--org-heading-content (headline)
  "Extract text content from org HEADLINE element.
Returns the content under the heading, excluding sub-headings."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline)))
    (when (and begin end)
      (save-excursion
        (goto-char begin)
        ;; Get content up to first sub-heading or end
        (let ((sub-heading (save-excursion
                            (re-search-forward "^\\*\\* " end t))))
          (string-trim
           (buffer-substring-no-properties
            begin
            (or sub-heading end))))))))

(defun agent--org-subheading-content (headline subheading-title)
  "Extract content of SUBHEADING-TITLE within HEADLINE.
Returns nil if subheading not found."
  (let ((begin (org-element-property :contents-begin headline))
        (end (org-element-property :contents-end headline)))
    (when (and begin end)
      (save-excursion
        (goto-char begin)
        (when (re-search-forward
               (format "^\\*\\* %s\n" (regexp-quote subheading-title))
               end t)
          (let ((content-start (point))
                (content-end (save-excursion
                              (if (re-search-forward "^\\*\\* " end t)
                                  (match-beginning 0)
                                end))))
            (string-trim
             (buffer-substring-no-properties content-start content-end))))))))

;;; Chat Reading

(defun agent-chat-read-pairs (n &optional include-think)
  "Read last N human/agent pairs from chat buffer.
If INCLUDE-THINK is non-nil, include Think headings.
Returns list of plists with :type, :content/:output, and optionally :think."
  (when-let* ((chat-info (agent-get :chat-pending))
              (buf (get-buffer (plist-get chat-info :buffer))))
    (with-current-buffer buf
      (let ((pairs '()))
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hl)
            (let ((title (org-element-property :raw-value hl))
                  (level (org-element-property :level hl)))
              (when (= level 1)
                (cond
                 ((string= title "Human Input")
                  (push (list :type :human
                              :content (agent--org-heading-content hl))
                        pairs))
                 ((string= title "Agent Response")
                  (push (list :type :agent
                              :think (when include-think
                                       (agent--org-subheading-content hl "Think"))
                              :output (agent--org-subheading-content hl "Output"))
                        pairs))))))
          nil nil 'headline)
        ;; Return last N pairs (pairs is in reverse order)
        (seq-take (nreverse pairs) (* 2 n))))))

(defun agent-chat-last-human-input ()
  "Return the most recent human input from chat buffer.
Convenience function for quick reads."
  (when-let* ((pairs (agent-chat-read-pairs 1)))
    (when-let* ((human (seq-find (lambda (p) (eq (plist-get p :type) :human))
                                 pairs)))
      (plist-get human :content))))

;;; Chat Writing

(defun agent-chat-append-response (think-text output-text)
  "Append agent response to chat buffer.
THINK-TEXT is the reasoning trace (collapsed).
OUTPUT-TEXT is the response to the human.
Adds a new Human Input heading for next turn."
  (when-let* ((chat-info (agent-get :chat-pending))
              (buf (get-buffer (plist-get chat-info :buffer))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "\n* Agent Response\n")
      (insert "** Think\n" think-text "\n\n")
      (insert "** Output\n" output-text "\n")
      (insert "\n* Human Input\n")
      ;; Fold the Think section
      (save-excursion
        (when (re-search-backward "^\\*\\* Think" nil t)
          (org-cycle)))
      ;; Save buffer if it has a file
      (when buffer-file-name
        (save-buffer)))))

(defun agent-chat-clear-pending ()
  "Clear chat pending flag after agent has responded."
  (agent-set :chat-pending nil))

;;; Chat Buffer Management

(defun agent-chat-pending-p ()
  "Return t if there is pending chat to handle."
  (not (null (agent-get :chat-pending))))

(defun agent-chat-buffer-name ()
  "Return the name of the pending chat buffer, or nil."
  (when-let* ((info (agent-get :chat-pending)))
    (plist-get info :buffer)))

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
        (insert "#+STARTUP: overview\n\n")
        (insert "* Human Input\n")))
    buf))

(provide 'agent-chat)
;;; agent-chat.el ends here
