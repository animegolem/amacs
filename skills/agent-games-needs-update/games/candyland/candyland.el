;;; candyland.el --- Candyland game implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents

;;; Commentary:

;; Simplified Candyland implementation for agent validation.
;; Players roll a color die and move to the next space of that color.

;;; Code:

(require 'agent-game-state)
(require 'agent-game-arbiter)
(require 'agent-game-audit)
(require 'agent-game-render)
(require 'cl-lib)

;;; Game Constants

(defconst candyland-colors '(red blue yellow green purple orange)
  "Available colors in Candyland.")

(defconst candyland-board-length 30
  "Length of the Candyland board.")

(defconst candyland-board-colors
  ;; Simple repeating color pattern for the board
  (let ((colors '()))
    (dotimes (i candyland-board-length)
      (push (nth (mod i (length candyland-colors)) candyland-colors) colors))
    (nreverse colors))
  "Color of each space on the board (1-indexed).")

;;; Board Definition

(defun candyland-board-get-color (position)
  "Get the color of space at POSITION (1-indexed)."
  (when (and (> position 0) (<= position candyland-board-length))
    (nth (1- position) candyland-board-colors)))

(defun candyland-board-find-next (position color)
  "Find next space of COLOR after POSITION.
Returns position number or nil if beyond board."
  (cl-loop for pos from (1+ position) to candyland-board-length
           when (eq (candyland-board-get-color pos) color)
           return pos))

;;; Game State Management

(defun candyland-init (players)
  "Initialize a new Candyland game with PLAYERS."
  (agent-game-state-init 'candyland players)
  (agent-game-state-set 'board
                        `((length . ,candyland-board-length)
                          (positions . ,(mapcar (lambda (p) (cons p 0)) players))
                          (last-roll . nil)))
  (agent-game-state-set 'phase 'playing)
  (agent-game-audit-init "candyland")
  (agent-game-audit-log 'game-init
                        `((players . ,players)
                          (board-length . ,candyland-board-length)))
  (message "Candyland game started with players: %s" players))

(defun candyland-get-position (player)
  "Get current position of PLAYER."
  (let* ((board (agent-game-state-get 'board))
         (positions (alist-get 'positions board)))
    (alist-get player positions)))

(defun candyland-set-position (player position)
  "Set PLAYER's position to POSITION."
  (let* ((board (agent-game-state-get 'board))
         (positions (alist-get 'positions board)))
    (setf (alist-get player positions) position)
    (setf (alist-get 'positions board) positions)
    (agent-game-state-set 'board board)))

(defun candyland-set-last-roll (color)
  "Set the last roll color."
  (let ((board (agent-game-state-get 'board)))
    (setf (alist-get 'last-roll board) color)
    (agent-game-state-set 'board board)))

;;; Game Actions

(defun candyland-roll-die ()
  "Roll the color die.
Returns a color symbol."
  (nth (random (length candyland-colors)) candyland-colors))

(defun candyland-do-roll (player)
  "Execute a roll action for PLAYER.
Returns result alist with roll and new position."
  (agent-game-arbiter-validate-turn player)

  (agent-game-arbiter-with-lock player
    (let* ((old-pos (candyland-get-position player))
           (roll (candyland-roll-die))
           (new-pos (candyland-board-find-next old-pos roll)))

      (candyland-set-last-roll roll)

      (if new-pos
          (progn
            ;; Move to new position
            (candyland-set-position player new-pos)
            (agent-game-audit-log 'move
                                  `((player . ,player)
                                    (roll . ,roll)
                                    (old-pos . ,old-pos)
                                    (new-pos . ,new-pos)))

            ;; Check for winner
            (when (>= new-pos candyland-board-length)
              (agent-game-state-set 'winner player)
              (agent-game-state-set 'phase 'finished)
              (agent-game-audit-log 'game-end
                                    `((winner . ,player)
                                      (final-position . ,new-pos))))

            (message "%s rolled %s: %d → %d%s"
                     player roll old-pos new-pos
                     (if (>= new-pos candyland-board-length) " WINS!" "")))

        ;; No space of that color ahead (shouldn't happen with our board)
        (agent-game-audit-log 'no-move
                              `((player . ,player)
                                (roll . ,roll)
                                (position . ,old-pos)))
        (message "%s rolled %s but cannot move" player roll))

      ;; Auto-advance turn unless game is over
      (unless (agent-game-state-get 'winner)
        (agent-game-arbiter-end-turn player))

      `((player . ,player)
        (roll . ,roll)
        (old-pos . ,old-pos)
        (new-pos . ,new-pos)))))

;;; User Commands

(defun agent-game-cmd-roll ()
  "Roll the die for the current player."
  (interactive)
  (let ((current (agent-game-state-get 'current-player)))
    (if current
        (progn
          (candyland-do-roll current)
          (agent-game-render-refresh))
      (message "No game in progress"))))

(defun agent-game-cmd-pass ()
  "Pass turn to next player."
  (interactive)
  (let ((current (agent-game-state-get 'current-player)))
    (if current
        (progn
          (agent-game-arbiter-end-turn current)
          (agent-game-render-refresh))
      (message "No game in progress"))))

;;; Game Start

;;;###autoload
(defun candyland-start (players)
  "Start a new Candyland game with PLAYERS.
PLAYERS is a list of symbols like '(human agent-1 agent-2)."
  (interactive
   (list (let ((input (read-string "Players (space-separated): " "human agent-1")))
           (mapcar #'intern (split-string input)))))
  (candyland-init players)
  (agent-game-render-show)
  (message "Candyland started! Current player: %s (Press 'r' to roll, '?' for help)"
           (agent-game-state-get 'current-player)))

(provide 'candyland)
;;; candyland.el ends here
