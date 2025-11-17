;;; dice-roller.el --- Dice rolling skill for agents -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents, skills

;;; Commentary:

;; This skill allows agents to roll the die in Candyland.
;; Agents should call candyland-agent-roll when it's their turn.

;;; Code:

(require 'candyland)

(defun candyland-agent-roll (agent-id)
  "Agent AGENT-ID rolls the die.
Returns alist with roll result and new position."
  (candyland-do-roll agent-id))

(provide 'dice-roller)
;;; dice-roller.el ends here
