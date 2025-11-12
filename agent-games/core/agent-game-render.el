;;; agent-game-render.el --- ASCII board rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents

;;; Commentary:

;; Renders game state as ASCII art in a buffer.
;; The buffer is read-only - all updates happen through game commands.

;;; Code:

(require 'agent-game-state)
(require 'cl-lib)

(defvar agent-game-render-buffer "*agent-game*"
  "Buffer name for game display.")

(defvar agent-game-render-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'agent-game-cmd-roll)
    (define-key map (kbd "p") 'agent-game-cmd-pass)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "a") 'agent-game-audit-view)
    (define-key map (kbd "?") 'agent-game-render-help)
    map)
  "Keymap for agent game render buffer.")

(define-derived-mode agent-game-render-mode special-mode "AgentGame"
  "Major mode for displaying agent games."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun agent-game-render-help ()
  "Show help for game commands."
  (interactive)
  (message "r: Roll | p: Pass turn | a: Audit log | q: Quit | ?: Help"))

(defun agent-game-render-get-buffer ()
  "Get or create the game render buffer."
  (let ((buf (get-buffer-create agent-game-render-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'agent-game-render-mode)
        (agent-game-render-mode)))
    buf))

(defun agent-game-render-clear ()
  "Clear the game render buffer."
  (let ((buf (agent-game-render-get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun agent-game-render-insert (text)
  "Insert TEXT into game render buffer."
  (let ((buf (agent-game-render-get-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

(defun agent-game-render-refresh ()
  "Refresh the game display."
  (agent-game-render-clear)

  ;; Header
  (agent-game-render-insert
   (propertize "═══════════════════════════════════════════════════════════════\n"
               'face 'bold))
  (agent-game-render-insert
   (propertize (format "  %s\n" (upcase (format "%s" (agent-game-state-get 'type))))
               'face '(:height 1.2 :weight bold)))
  (agent-game-render-insert
   (propertize "═══════════════════════════════════════════════════════════════\n\n"
               'face 'bold))

  ;; Game state info
  (agent-game-render-insert
   (format "Turn: %d  |  Phase: %s  |  Current Player: %s\n\n"
           (or (agent-game-state-get 'turn-count) 0)
           (agent-game-state-get 'phase)
           (propertize (format "%s" (agent-game-state-get 'current-player))
                      'face 'highlight)))

  ;; Render game-specific content
  (let ((type (agent-game-state-get 'type)))
    (pcase type
      ('candyland (agent-game-render-candyland))
      (_ (agent-game-render-insert "Unknown game type\n"))))

  ;; Footer with controls
  (agent-game-render-insert
   (propertize "\n───────────────────────────────────────────────────────────────\n"
               'face 'shadow))
  (agent-game-render-insert
   (propertize "  [r] Roll  [p] Pass  [a] Audit  [q] Quit  [?] Help\n"
               'face 'shadow))

  ;; Show winner if game is over
  (when-let ((winner (agent-game-state-get 'winner)))
    (agent-game-render-insert
     (propertize (format "\n🎉 %s WINS! 🎉\n" winner)
                'face '(:foreground "green" :weight bold))))

  (with-current-buffer (agent-game-render-get-buffer)
    (goto-char (point-min))))

(defun agent-game-render-candyland ()
  "Render Candyland-specific board."
  (let* ((board (agent-game-state-get 'board))
         (positions (alist-get 'positions board))
         (last-roll (alist-get 'last-roll board))
         (board-length (or (alist-get 'length board) 30)))

    ;; Show last roll
    (when last-roll
      (agent-game-render-insert
       (format "Last Roll: %s\n\n"
               (propertize (format "%s" last-roll)
                          'face `(:foreground ,(agent-game-color-to-hex last-roll)
                                  :weight bold)))))

    ;; Render board
    (agent-game-render-insert "Board:\n")
    (agent-game-render-insert "START ")

    ;; Simple linear board representation
    (dotimes (i board-length)
      (let* ((pos (1+ i))
             (players-here (cl-loop for (player . player-pos) in positions
                                   when (= player-pos pos)
                                   collect player)))
        (cond
         (players-here
          ;; Show player(s) at this position
          (agent-game-render-insert
           (propertize (format "[%s]" (mapconcat #'symbol-name players-here ","))
                      'face 'highlight)))
         ((zerop (mod pos 5))
          ;; Milestone markers
          (agent-game-render-insert
           (propertize (format "<%d>" pos) 'face 'bold)))
         (t
          ;; Regular space
          (agent-game-render-insert "-"))))
      (agent-game-render-insert " "))

    (agent-game-render-insert "FINISH\n\n")

    ;; Player positions
    (agent-game-render-insert "Players:\n")
    (dolist (player-pos positions)
      (agent-game-render-insert
       (format "  %s: Position %d\n"
               (propertize (format "%s" (car player-pos))
                          'face (when (equal (car player-pos)
                                           (agent-game-state-get 'current-player))
                                  'highlight))
               (cdr player-pos))))))

(defun agent-game-color-to-hex (color)
  "Convert COLOR symbol to hex color code."
  (pcase color
    ('red "#ff0000")
    ('blue "#0000ff")
    ('yellow "#ffff00")
    ('green "#00ff00")
    ('purple "#800080")
    ('orange "#ff8800")
    (_ "#ffffff")))

(defun agent-game-render-show ()
  "Show the game render buffer."
  (interactive)
  (let ((buf (agent-game-render-get-buffer)))
    (agent-game-render-refresh)
    (display-buffer buf)))

(provide 'agent-game-render)
;;; agent-game-render.el ends here
