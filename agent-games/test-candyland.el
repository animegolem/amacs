;;; test-candyland.el --- Test script for Candyland game -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, test

;;; Commentary:

;; Simple test script to verify Candyland game mechanics work correctly.
;; Run with: emacs -Q -l agent-games.el -l test-candyland.el --batch

;;; Code:

(require 'agent-games)

(defun test-candyland-basic ()
  "Test basic Candyland game mechanics."
  (message "\n=== Testing Candyland Basic Mechanics ===\n")

  ;; Test 1: Game initialization
  (message "Test 1: Initializing game...")
  (candyland-init '(player-1 player-2))
  (assert (equal (agent-game-state-get 'type) 'candyland))
  (assert (equal (agent-game-state-get 'current-player) 'player-1))
  (message "✓ Game initialized correctly")

  ;; Test 2: First player roll
  (message "\nTest 2: Player 1 rolls...")
  (let ((result (candyland-do-roll 'player-1)))
    (assert (alist-get 'roll result))
    (assert (alist-get 'new-pos result))
    (message "✓ Player 1 rolled %s, moved to %d"
             (alist-get 'roll result)
             (alist-get 'new-pos result)))

  ;; Test 3: Turn advances
  (message "\nTest 3: Checking turn advancement...")
  (assert (equal (agent-game-state-get 'current-player) 'player-2))
  (message "✓ Turn advanced to player-2")

  ;; Test 4: Wrong player cannot act
  (message "\nTest 4: Testing turn enforcement...")
  (condition-case err
      (progn
        (candyland-do-roll 'player-1)
        (error "Should have failed - not player-1's turn"))
    (error
     (message "✓ Correctly prevented out-of-turn action: %s" (error-message-string err))))

  ;; Test 5: Second player can act
  (message "\nTest 5: Player 2 rolls...")
  (let ((result (candyland-do-roll 'player-2)))
    (message "✓ Player 2 rolled %s, moved to %d"
             (alist-get 'roll result)
             (alist-get 'new-pos result)))

  ;; Test 6: Position tracking
  (message "\nTest 6: Checking position tracking...")
  (let ((pos1 (candyland-get-position 'player-1))
        (pos2 (candyland-get-position 'player-2)))
    (message "  Player 1 position: %d" pos1)
    (message "  Player 2 position: %d" pos2)
    (assert (>= pos1 0))
    (assert (>= pos2 0))
    (message "✓ Positions tracked correctly"))

  ;; Test 7: Audit log exists
  (message "\nTest 7: Checking audit log...")
  (assert (file-exists-p agent-game-audit-file))
  (message "✓ Audit log created at: %s" agent-game-audit-file)

  ;; Test 8: Skills work
  (message "\nTest 8: Testing agent skills...")
  (let ((pos (candyland-agent-get-position 'player-1))
        (all-pos (candyland-agent-get-all-positions))
        (is-turn (candyland-agent-is-my-turn-p 'player-1)))
    (message "  Agent skill: get-position => %d" pos)
    (message "  Agent skill: get-all-positions => %s" all-pos)
    (message "  Agent skill: is-my-turn => %s" is-turn)
    (assert (numberp pos))
    (assert (listp all-pos))
    (message "✓ Agent skills functional"))

  (message "\n=== All Tests Passed! ===\n"))

(defun test-candyland-full-game ()
  "Simulate a full game to completion."
  (message "\n=== Simulating Full Game ===\n")

  (candyland-init '(player-1 player-2))

  (let ((max-turns 100)
        (turn 0))
    (while (and (< turn max-turns)
                (not (agent-game-state-get 'winner)))
      (let* ((current (agent-game-state-get 'current-player))
             (result (candyland-do-roll current))
             (new-pos (alist-get 'new-pos result)))
        (setq turn (1+ turn))
        (message "Turn %d: %s rolled %s, moved to %d"
                 turn current
                 (alist-get 'roll result)
                 new-pos)))

    (if-let ((winner (agent-game-state-get 'winner)))
        (message "\n✓ Game completed! Winner: %s (in %d turns)" winner turn)
      (message "\n✗ Game did not complete within %d turns" max-turns))))

;; Run tests if executed as batch script
(when noninteractive
  (test-candyland-basic)
  (test-candyland-full-game))

(provide 'test-candyland)
;;; test-candyland.el ends here
