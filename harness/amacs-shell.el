;;; amacs-shell.el --- Comint-based human-agent I/O -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Comint-based shell for human-agent interaction.
;; Replaces org-mode prompt blocks with a simple shell interface.
;;
;; The human types at a prompt, presses enter, and the agent's
;; response appears inline. No subprocess is used - we intercept
;; input via `comint-input-sender' and insert responses directly.
;;
;; See: AI-IMP-036-comint-shell
;;      RAG/RFC/amacs-rfc-v4-transition.md

;;; Code:

(require 'comint)

;;; Variables

(defvar amacs-shell-prompt "amacs> "
  "Prompt string for the AMACS shell.")

(defvar amacs-shell-buffer-name "*amacs-shell*"
  "Name of the AMACS shell buffer.")

(defvar amacs-shell--pending-input nil
  "Most recent human input, waiting to be processed.")

(defvar amacs-shell--processing nil
  "Non-nil when inference is in progress.")

;;; Mode Definition

(defvar amacs-shell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from comint-mode-map
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `amacs-shell-mode'.")

(define-derived-mode amacs-shell-mode comint-mode "AMACS"
  "Major mode for AMACS human-agent interaction.

This is a comint-based shell with no real subprocess.
Human input is captured and processed by the AMACS harness.

\\{amacs-shell-mode-map}"
  ;; No real process - we fake it
  (setq-local comint-input-sender #'amacs-shell--input-sender)
  (setq-local comint-process-echoes nil)
  ;; Prompt handling
  (setq-local comint-prompt-regexp (concat "^" (regexp-quote amacs-shell-prompt)))
  (setq-local comint-prompt-read-only t)
  ;; Don't try to send to a process
  (setq-local comint-input-autoexpand nil))

;;; Fake Process Setup

(defun amacs-shell--setup-fake-process (buffer)
  "Set up a fake process for BUFFER so comint functions work."
  (let ((fake-proc (start-process "amacs-fake" buffer "cat")))
    ;; Immediately stop cat from doing anything
    (set-process-query-on-exit-flag fake-proc nil)
    (set-process-filter fake-proc #'amacs-shell--process-filter)
    fake-proc))

(defun amacs-shell--process-filter (_proc _output)
  "Filter for fake process. Ignores all output from cat."
  ;; We don't want cat to echo anything
  nil)

;;; Input Handling

(defun amacs-shell--input-sender (_proc input)
  "Handle INPUT from comint. PROC is ignored (fake process).
Stores input for harness to process and triggers inference."
  (setq amacs-shell--pending-input (string-trim input))
  ;; Don't send to process (there isn't really one)
  ;; Instead, trigger our inference pipeline
  (amacs-shell--trigger-inference))

(defun amacs-shell--trigger-inference ()
  "Trigger inference for pending input.
For now, just shows a placeholder. Full inference in IMP-037."
  (when (and amacs-shell--pending-input
             (not (string-empty-p amacs-shell--pending-input))
             (not amacs-shell--processing))
    (setq amacs-shell--processing t)
    ;; Show thinking indicator
    (amacs-shell--insert-output "\n[Processing...]\n")
    ;; TODO: Actual inference call goes here (IMP-037)
    ;; For now, just show placeholder response
    (run-at-time 0.1 nil #'amacs-shell--placeholder-response)))

(defun amacs-shell--placeholder-response ()
  "Insert placeholder response. Replaced by real inference in IMP-037."
  (amacs-shell--insert-response
   (format "I received: \"%s\"\n\n(This is a placeholder. Real inference coming in IMP-037.)"
           amacs-shell--pending-input))
  (setq amacs-shell--pending-input nil)
  (setq amacs-shell--processing nil))

;;; Output Handling

(defun amacs-shell--insert-output (text)
  "Insert TEXT into the shell buffer at process mark."
  (let ((buf (get-buffer amacs-shell-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          (goto-char (process-mark proc))
          (insert text)
          (set-marker (process-mark proc) (point)))))))

(defun amacs-shell--insert-response (response)
  "Insert agent RESPONSE and new prompt."
  (let ((buf (get-buffer amacs-shell-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          ;; Clear the [Processing...] line
          (goto-char (process-mark proc))
          (delete-region (line-beginning-position) (point))
          ;; Insert response
          (insert response)
          (insert "\n\n")
          ;; Insert new prompt
          (let ((prompt-start (point)))
            (insert amacs-shell-prompt)
            (add-text-properties prompt-start (point)
                                 '(read-only t rear-nonsticky t front-sticky (read-only)))
            (set-marker (process-mark proc) (point))))))))

;;; Public Interface

;;;###autoload
(defun amacs-shell-start ()
  "Start or switch to the AMACS shell buffer."
  (interactive)
  (let ((buf (get-buffer-create amacs-shell-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'amacs-shell-mode)
        (amacs-shell-mode)
        ;; Set up fake process
        (amacs-shell--setup-fake-process buf)
        ;; Insert initial prompt
        (let ((inhibit-read-only t)
              (proc (get-buffer-process buf)))
          (erase-buffer)
          (insert "Welcome to AMACS shell.\n")
          (insert "Type your message and press RET to send.\n\n")
          (let ((prompt-start (point)))
            (insert amacs-shell-prompt)
            (add-text-properties prompt-start (point)
                                 '(read-only t rear-nonsticky t front-sticky (read-only)))
            (set-marker (process-mark proc) (point))))))
    (pop-to-buffer buf)
    buf))

(defun amacs-shell-get-pending-input ()
  "Return the pending human input, if any."
  amacs-shell--pending-input)

(defun amacs-shell-clear-pending-input ()
  "Clear the pending input after processing."
  (setq amacs-shell--pending-input nil))

(provide 'amacs-shell)
;;; amacs-shell.el ends here
