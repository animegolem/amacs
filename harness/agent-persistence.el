;;; agent-persistence.el --- Serialization for AMACS state -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Persistence layer for AMACS v4 shell.
;; Handles serialization of chat history and scratchpad to org files.
;;
;; See: AI-IMP-039-serialization
;;      RAG/RFC/amacs-rfc-v4-transition.md

;;; Code:

(require 'cl-lib)

;;; Variables

(defvar agent-persistence-directory "~/.agent/"
  "Directory for AMACS persistent state.")

(defvar agent-chat-file nil
  "Path to agent-chat.org file. Set during init.")

(defvar agent-scratchpad-file nil
  "Path to scratchpad.org file. Set during init.")

;;; Initialization

(defun agent-persistence-init ()
  "Initialize persistence layer, ensuring directory and files exist."
  (let ((dir (expand-file-name agent-persistence-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setq agent-chat-file (expand-file-name "agent-chat.org" dir))
    (setq agent-scratchpad-file (expand-file-name "scratchpad.org" dir))
    ;; Create files if they don't exist
    (unless (file-exists-p agent-chat-file)
      (with-temp-file agent-chat-file
        (insert "#+TITLE: AMACS Chat History\n\n")))
    (unless (file-exists-p agent-scratchpad-file)
      (with-temp-file agent-scratchpad-file
        (insert "#+TITLE: AMACS Scratchpad\n\n")))))

;;; Chat Serialization

(defun agent-chat-append-exchange (tick human-msg agent-reply)
  "Append exchange to agent-chat.org.
TICK is the tick number, HUMAN-MSG the human input, AGENT-REPLY the response."
  (unless agent-chat-file
    (agent-persistence-init))
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
    (with-temp-buffer
      (insert (format "* Tick %d\n" tick))
      (insert ":PROPERTIES:\n")
      (insert (format ":TIMESTAMP: %s\n" timestamp))
      (insert ":END:\n\n")
      (insert "** Human\n")
      (insert human-msg)
      (insert "\n\n")
      (insert "** Agent\n")
      (insert agent-reply)
      (insert "\n\n")
      (append-to-file (point-min) (point-max) agent-chat-file))))

(defun agent-chat-load-history ()
  "Load chat history from agent-chat.org.
Returns list of plists: ((:human STR :agent STR :tick N) ...)
Ordered oldest to newest."
  (unless agent-chat-file
    (agent-persistence-init))
  (if (not (file-exists-p agent-chat-file))
      nil
    (with-temp-buffer
      (insert-file-contents agent-chat-file)
      (goto-char (point-min))
      (let ((exchanges nil))
        ;; Find each Tick heading
        (while (re-search-forward "^\\* Tick \\([0-9]+\\)" nil t)
          (let ((tick (string-to-number (match-string 1)))
                (section-start (point))
                (section-end (save-excursion
                               (if (re-search-forward "^\\* " nil t)
                                   (match-beginning 0)
                                 (point-max))))
                human-content agent-content)
            ;; Find Human subsection
            (goto-char section-start)
            (when (re-search-forward "^\\*\\* Human\n" section-end t)
              (let ((content-start (point))
                    (content-end (save-excursion
                                   (if (re-search-forward "^\\*\\* " section-end t)
                                       (match-beginning 0)
                                     section-end))))
                (setq human-content
                      (string-trim (buffer-substring-no-properties
                                    content-start content-end)))))
            ;; Find Agent subsection
            (goto-char section-start)
            (when (re-search-forward "^\\*\\* Agent\n" section-end t)
              (let ((content-start (point))
                    (content-end (save-excursion
                                   (if (re-search-forward "^\\*\\* " section-end t)
                                       (match-beginning 0)
                                     section-end))))
                (setq agent-content
                      (string-trim (buffer-substring-no-properties
                                    content-start content-end)))))
            ;; Record if we found both
            (when (and human-content agent-content)
              (push (list :human human-content
                          :agent agent-content
                          :tick tick)
                    exchanges))
            (goto-char section-end)))
        ;; Return oldest first (reverse the push order)
        (nreverse exchanges)))))

;;; Scratchpad Serialization

(defun agent-scratchpad-append (heading thread-id tick content)
  "Append CONTENT under HEADING in scratchpad.org.
THREAD-ID is the thread (or nil for global), TICK is the tick number.
If heading exists, append to it. Otherwise create new heading."
  (unless agent-scratchpad-file
    (agent-persistence-init))
  (let ((thread-prop (if thread-id thread-id "null")))
    (with-current-buffer (find-file-noselect agent-scratchpad-file)
      (goto-char (point-min))
      ;; Look for existing heading with matching name
      (if (re-search-forward (format "^\\* %s\n" (regexp-quote heading)) nil t)
          ;; Found - go to end of this section and append
          (let ((section-end (save-excursion
                               (if (re-search-forward "^\\* " nil t)
                                   (match-beginning 0)
                                 (point-max)))))
            (goto-char section-end)
            ;; Back up past any trailing whitespace
            (skip-chars-backward " \t\n")
            (insert "\n\n" content))
        ;; Not found - create new heading at end
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "* %s\n" heading))
        (insert ":PROPERTIES:\n")
        (insert (format ":THREAD: %s\n" thread-prop))
        (insert (format ":TICK: %d\n" tick))
        (insert ":END:\n\n")
        (insert content))
      (save-buffer)
      (kill-buffer))))

(defun agent-scratchpad-load ()
  "Load scratchpad from scratchpad.org.
Returns list of plists with :heading, :thread, :tick, :content.
Ordered as they appear in file."
  (unless agent-scratchpad-file
    (agent-persistence-init))
  (if (not (file-exists-p agent-scratchpad-file))
      nil
    (with-temp-buffer
      (insert-file-contents agent-scratchpad-file)
      (goto-char (point-min))
      (let ((notes nil))
        ;; Find each heading
        (while (re-search-forward "^\\* \\(.+\\)\n" nil t)
          (let ((heading (match-string 1))
                (section-end (save-excursion
                               (if (re-search-forward "^\\* " nil t)
                                   (match-beginning 0)
                                 (point-max))))
                thread-id tick content)
            ;; Parse properties if present
            (when (looking-at ":PROPERTIES:\n")
              (let ((props-end (save-excursion
                                 (re-search-forward ":END:\n" section-end t))))
                (when props-end
                  ;; Extract THREAD property
                  (save-excursion
                    (when (re-search-forward "^:THREAD: \\(.+\\)$" props-end t)
                      (let ((val (match-string 1)))
                        (setq thread-id (unless (string= val "null") val)))))
                  ;; Extract TICK property
                  (save-excursion
                    (when (re-search-forward "^:TICK: \\([0-9]+\\)$" props-end t)
                      (setq tick (string-to-number (match-string 1)))))
                  (goto-char props-end))))
            ;; Content is everything after properties until next heading
            (setq content (string-trim
                           (buffer-substring-no-properties (point) section-end)))
            (push (list :heading heading
                        :thread thread-id
                        :tick tick
                        :content content)
                  notes)
            (goto-char section-end)))
        ;; Return in file order
        (nreverse notes)))))

(defun agent-scratchpad-filter-by-thread (notes thread-id)
  "Filter NOTES to include global notes and those matching THREAD-ID."
  (seq-filter (lambda (note)
                (let ((note-thread (plist-get note :thread)))
                  (or (null note-thread)  ; global
                      (equal note-thread thread-id))))
              notes))

(provide 'agent-persistence)
;;; agent-persistence.el ends here
