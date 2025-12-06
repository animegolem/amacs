;;; agent-game-memory.el --- Agent memory isolation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents

;;; Commentary:

;; Each agent has its own isolated memory space.
;; Agents can only read/write their own memory, not other agents' memory.
;; This prevents cheating and enforces proper information hiding.

;;; Code:

(require 'cl-lib)

(defvar agent-game-memory-root
  (expand-file-name "agent-games" user-emacs-directory)
  "Root directory for all agent game data.")

(defun agent-game-memory-path (game-type agent-id &optional subpath)
  "Get path to AGENT-ID's memory for GAME-TYPE.
If SUBPATH is provided, append it to the agent's memory directory."
  (let ((base (expand-file-name
               (format "games/%s/agents/%s/" game-type agent-id)
               agent-game-memory-root)))
    (if subpath
        (expand-file-name subpath base)
      base)))

(defun agent-game-memory-init (game-type agent-id)
  "Initialize memory directories for AGENT-ID in GAME-TYPE."
  (let ((memory-dir (agent-game-memory-path game-type agent-id "memory")))
    (make-directory memory-dir t)
    ;; Create initial memory files
    (agent-game-memory-write game-type agent-id "memory/game-state.org"
                             (format "#+TITLE: %s's Game State - %s\n#+DATE: %s\n\n* Current Position\n0\n\n* Game History\n"
                                    agent-id game-type
                                    (format-time-string "%Y-%m-%d")))
    (agent-game-memory-write game-type agent-id "scratch.org"
                             (format "#+TITLE: %s's Scratch Notes\n#+DATE: %s\n\n"
                                    agent-id
                                    (format-time-string "%Y-%m-%d")))))

(defun agent-game-memory-read (game-type agent-id file)
  "Read FILE from AGENT-ID's memory for GAME-TYPE.
FILE is relative to agent's memory directory."
  (let ((filepath (agent-game-memory-path game-type agent-id file)))
    (when (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (buffer-string)))))

(defun agent-game-memory-write (game-type agent-id file content)
  "Write CONTENT to FILE in AGENT-ID's memory for GAME-TYPE.
FILE is relative to agent's memory directory."
  (let* ((filepath (agent-game-memory-path game-type agent-id file))
         (dir (file-name-directory filepath)))
    (make-directory dir t)
    (with-temp-file filepath
      (insert content))))

(defun agent-game-memory-append (game-type agent-id file content)
  "Append CONTENT to FILE in AGENT-ID's memory for GAME-TYPE."
  (let ((existing (or (agent-game-memory-read game-type agent-id file) "")))
    (agent-game-memory-write game-type agent-id file
                            (concat existing content))))

(defun agent-game-memory-update-position (game-type agent-id position)
  "Update AGENT-ID's recorded position in memory."
  (agent-game-memory-append
   game-type agent-id "memory/game-state.org"
   (format "\n** [%s] Position updated: %d"
           (format-time-string "%H:%M:%S")
           position)))

(defun agent-game-memory-log-action (game-type agent-id action details)
  "Log an ACTION with DETAILS to AGENT-ID's history."
  (agent-game-memory-append
   game-type agent-id "memory/game-state.org"
   (format "\n** [%s] %s: %s"
           (format-time-string "%H:%M:%S")
           action
           details)))

(provide 'agent-game-memory)
;;; agent-game-memory.el ends here
