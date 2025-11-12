;;; position-tracker.el --- Position tracking skill for agents -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents, skills

;;; Commentary:

;; This skill allows agents to query their position and other game state.
;; All data comes from shared game state (read-only for agents).

;;; Code:

(require 'candyland)

(defun candyland-agent-get-position (agent-id)
  "Get AGENT-ID's current position."
  (candyland-get-position agent-id))

(defun candyland-agent-get-all-positions ()
  "Get all players' positions as an alist."
  (let* ((board (agent-game-state-get 'board)))
    (alist-get 'positions board)))

(defun candyland-agent-get-last-roll ()
  "Get the last roll color."
  (let ((board (agent-game-state-get 'board)))
    (alist-get 'last-roll board)))

(defun candyland-agent-is-my-turn-p (agent-id)
  "Return t if it's AGENT-ID's turn."
  (equal agent-id (agent-game-state-get 'current-player)))

(defun candyland-agent-get-winner ()
  "Get the winner, or nil if game is not over."
  (agent-game-state-get 'winner))

(provide 'position-tracker)
;;; position-tracker.el ends here
