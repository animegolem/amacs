;;; amacs-hub.el --- AMACS Hub Dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Unified dashboard for AMACS agent state using magit-section.
;; Shows threads, buffers, skills, chat, monologue, and scratchpad.
;;
;; Key bindings:
;;   TAB - Expand/collapse section inline
;;   g   - Refresh hub
;;   RET - Jump to source (IMP-030)
;;
;; Usage:
;;   M-x amacs-hub
;;
;; See: AI-IMP-029, RFC Part 21

;;; Code:

(require 'magit-section)
(require 'agent-consciousness)
(require 'agent-threads)
(require 'agent-tick)
(require 'agent-chat)
(require 'agent-scratchpad)
(require 'agent-skills)

;; Forward declare to avoid circular require
(declare-function agent-think "agent-inference")

;;; Section Classes

(defclass amacs-hub-section (magit-section) ())
(defclass amacs-status-section (magit-section) ())
(defclass amacs-threads-section (magit-section) ())
(defclass amacs-thread-section (magit-section) ())
(defclass amacs-buffers-section (magit-section) ())
(defclass amacs-buffer-section (magit-section) ())
(defclass amacs-skills-section (magit-section) ())
(defclass amacs-skill-section (magit-section) ())
(defclass amacs-chat-section (magit-section) ())
(defclass amacs-chat-buffer-section (magit-section) ())
(defclass amacs-chat-tick-section (magit-section) ())
(defclass amacs-monologue-section (magit-section) ())
(defclass amacs-monologue-tick-section (magit-section) ())
(defclass amacs-scratchpad-section (magit-section) ())
(defclass amacs-scratchpad-buffer-section (magit-section) ())
(defclass amacs-scratchpad-heading-section (magit-section) ())

;;; Variables

(defvar amacs-hub-buffer-name "*amacs-hub*"
  "Name of the hub buffer.")

;;; Mode Definition

(defvar amacs-hub-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'amacs-hub-refresh)
    (define-key map (kbd "RET") #'amacs-hub-visit-thing-at-point)
    ;; Action keybindings (IMP-031)
    (define-key map (kbd "a") #'amacs-hub-add)
    (define-key map (kbd "k") #'amacs-hub-remove)
    (define-key map (kbd "c") #'amacs-hub-complete-thread)
    (define-key map (kbd "+") #'amacs-hub-increase-scratchpad-depth)
    (define-key map (kbd "-") #'amacs-hub-decrease-scratchpad-depth)
    (define-key map (kbd "t") #'amacs-hub-trigger-tick)
    (define-key map (kbd "T") #'amacs-hub-trigger-tick-with-thinking)
    (define-key map (kbd "s") #'amacs-hub-switch-thread)
    (define-key map (kbd "?") #'amacs-hub-help)
    ;; API settings keybindings (IMP-032)
    (define-key map (kbd "M-t") #'amacs-hub-toggle-thinking)
    (define-key map (kbd "M-T") #'amacs-hub-set-temperature)
    (define-key map (kbd "M-p") #'amacs-hub-set-top-p)
    (define-key map (kbd "M-m") #'amacs-hub-set-model)
    map)
  "Keymap for `amacs-hub-mode'.")

(define-derived-mode amacs-hub-mode magit-section-mode "AMACS-Hub"
  "Major mode for AMACS hub dashboard."
  :group 'amacs
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (amacs-hub-refresh))))

;;; Navigation (IMP-030)

(defun amacs-hub--ensure-other-window ()
  "Ensure we have another window to use, splitting if needed."
  (when (one-window-p)
    (split-window-right))
  (other-window 1))

(defun amacs-hub--visit-thread (section)
  "Switch to thread in SECTION."
  (let* ((value (oref section value))
         (thread-id (if (listp value) (alist-get 'id value) value)))
    (if thread-id
        (progn
          (agent-switch-thread thread-id)
          (amacs-hub-refresh)
          (message "Switched to thread: %s" thread-id))
      (message "No thread to switch to"))))

(defun amacs-hub--visit-buffer (section)
  "Open buffer from SECTION in other window."
  (let ((buf-name (oref section value)))
    (if (and buf-name (get-buffer buf-name))
        (progn
          (amacs-hub--ensure-other-window)
          (switch-to-buffer buf-name))
      (message "Buffer not found: %s (try 'g' to refresh)" buf-name))))

(defun amacs-hub--visit-skill (section)
  "Open SKILL.md from SECTION in other window."
  (let* ((skill-name (oref section value))
         (skill-path (expand-file-name
                      (format "~/.agent/skills/%s/SKILL.md" skill-name))))
    (if (file-exists-p skill-path)
        (progn
          (amacs-hub--ensure-other-window)
          (find-file skill-path))
      (message "Skill file not found: %s" skill-path))))

(defun amacs-hub--visit-chat-tick (section)
  "Jump to chat tick heading from SECTION in other window."
  (let* ((value (oref section value))
         (buf (alist-get 'buffer value))
         (tick-num (alist-get 'tick value)))
    (if (and buf (buffer-live-p buf))
        (progn
          (amacs-hub--ensure-other-window)
          (switch-to-buffer buf)
          (goto-char (point-min))
          (if (re-search-forward (format "^\\* Tick %d" tick-num) nil t)
              (beginning-of-line)
            (message "Tick %d heading not found" tick-num)))
      (message "Chat buffer not available (try 'g' to refresh)"))))

(defun amacs-hub--visit-monologue (section)
  "Jump to monologue entry from SECTION in other window."
  (let* ((value (oref section value))
         (tick-num (alist-get 'tick value))
         (file (expand-file-name "~/.agent/monologue.org")))
    (if (file-exists-p file)
        (progn
          (amacs-hub--ensure-other-window)
          (find-file file)
          (goto-char (point-min))
          (if (re-search-forward (format "\\[TICK %d\\]" tick-num) nil t)
              (beginning-of-line)
            (message "Tick %d entry not found" tick-num)))
      (message "Monologue file not found"))))

(defun amacs-hub--visit-scratchpad (section)
  "Jump to scratchpad heading from SECTION in other window."
  (let* ((value (oref section value))
         (buf (alist-get 'buffer value))
         (begin (alist-get 'begin value))
         (title (alist-get 'title value)))
    (if (and buf (buffer-live-p buf))
        (progn
          (amacs-hub--ensure-other-window)
          (switch-to-buffer buf)
          (if begin
              (goto-char begin)
            ;; Fallback: search for heading by title
            (goto-char (point-min))
            (unless (re-search-forward (format "^\\*+ %s" (regexp-quote title)) nil t)
              (message "Heading '%s' not found" title))))
      (message "Scratchpad buffer not available (try 'g' to refresh)"))))

(defun amacs-hub-visit-thing-at-point ()
  "Visit the item at point in other window.
RET on different sections does different things:
- Thread: switch to that thread
- Buffer: open buffer in other window
- Skill: open SKILL.md in other window
- Chat tick: jump to tick heading in chat buffer
- Monologue: jump to entry in monologue.org
- Scratchpad: jump to heading in scratchpad buffer"
  (interactive)
  (let ((section (magit-current-section)))
    (when section
      (pcase (eieio-object-class section)
        ('amacs-thread-section (amacs-hub--visit-thread section))
        ('amacs-buffer-section (amacs-hub--visit-buffer section))
        ('amacs-skill-section (amacs-hub--visit-skill section))
        ('amacs-chat-tick-section (amacs-hub--visit-chat-tick section))
        ('amacs-monologue-tick-section (amacs-hub--visit-monologue section))
        ('amacs-scratchpad-heading-section (amacs-hub--visit-scratchpad section))
        (_ (message "No action for this section"))))))

;;; Actions (IMP-031)

(defun amacs-hub-add ()
  "Add something based on current section context.
In Threads section: create new thread.
In Buffers section: add buffer to active thread.
In Skills section: bind skill to active thread."
  (interactive)
  (let* ((section (magit-current-section))
         (section-class (when section (eieio-object-class section)))
         (parent (when section (oref section parent)))
         (parent-class (when parent (eieio-object-class parent))))
    (cond
     ;; In threads section or on a thread
     ((or (eq section-class 'amacs-threads-section)
          (eq section-class 'amacs-thread-section)
          (eq parent-class 'amacs-threads-section))
      (amacs-hub--add-thread))
     ;; In buffers section or on a buffer
     ((or (eq section-class 'amacs-buffers-section)
          (eq section-class 'amacs-buffer-section)
          (eq parent-class 'amacs-buffers-section))
      (amacs-hub--add-buffer))
     ;; In skills section or on a skill
     ((or (eq section-class 'amacs-skills-section)
          (eq section-class 'amacs-skill-section)
          (eq parent-class 'amacs-skills-section))
      (amacs-hub--add-skill))
     (t
      (user-error "Press 'a' in Threads, Buffers, or Skills section to add")))))

(defun amacs-hub--add-thread ()
  "Create a new thread with prompted concern."
  (let ((concern (read-string "Thread concern: ")))
    (when (and concern (not (string-empty-p concern)))
      (let ((thread (agent-create-thread concern)))
        (agent-add-thread thread t)  ; t = activate
        (amacs-hub-refresh)
        (message "Created and activated thread: %s" (alist-get 'id thread))))))

(defun amacs-hub--add-buffer ()
  "Add a buffer to the active thread."
  (let ((active-id (agent-active-thread)))
    (unless active-id
      (user-error "No active thread"))
    (let* ((all-buffers (mapcar #'buffer-name (buffer-list)))
           (buf-name (completing-read "Add buffer: " all-buffers nil t)))
      (when buf-name
        (agent-thread-add-buffer active-id buf-name)
        (amacs-hub-refresh)
        (message "Added buffer '%s' to thread" buf-name)))))

(defun amacs-hub--add-skill ()
  "Bind a skill to the active thread."
  (let ((active-id (agent-active-thread)))
    (unless active-id
      (user-error "No active thread"))
    (let* ((available (agent-list-available-skills))
           (skill-name (completing-read "Bind skill: " available nil t)))
      (when skill-name
        (agent-bind-skill-to-thread skill-name)
        (amacs-hub-refresh)
        (message "Bound skill '%s' to thread" skill-name)))))

(defun amacs-hub-remove ()
  "Remove/archive based on current section context.
On thread: archive thread.
On buffer: remove buffer from thread.
On skill: unbind skill from thread."
  (interactive)
  (let* ((section (magit-current-section))
         (section-class (when section (eieio-object-class section))))
    (cond
     ((eq section-class 'amacs-thread-section)
      (amacs-hub--archive-thread section))
     ((eq section-class 'amacs-buffer-section)
      (amacs-hub--remove-buffer section))
     ((eq section-class 'amacs-skill-section)
      (amacs-hub--unbind-skill section))
     (t
      (user-error "Press 'k' on a thread, buffer, or skill to remove")))))

(defun amacs-hub--archive-thread (section)
  "Archive the thread in SECTION."
  (let* ((value (oref section value))
         (thread-id (if (listp value) (alist-get 'id value) value)))
    (when thread-id
      (when (yes-or-no-p (format "Archive thread '%s'? " thread-id))
        (agent-archive-thread thread-id)
        (amacs-hub-refresh)
        (message "Archived thread: %s" thread-id)))))

(defun amacs-hub--remove-buffer (section)
  "Remove the buffer in SECTION from active thread."
  (let* ((buf-name (oref section value))
         (active-id (agent-active-thread)))
    (when (and buf-name active-id)
      (agent-thread-remove-buffer active-id buf-name)
      (amacs-hub-refresh)
      (message "Removed buffer '%s' from thread" buf-name))))

(defun amacs-hub--unbind-skill (section)
  "Unbind the skill in SECTION from active thread."
  (let ((skill-name (oref section value)))
    (when skill-name
      (agent-unbind-skill-from-thread skill-name)
      (amacs-hub-refresh)
      (message "Unbound skill '%s' from thread" skill-name))))

(defun amacs-hub-complete-thread ()
  "Complete the current or selected thread with evidence."
  (interactive)
  (let* ((section (magit-current-section))
         (section-class (when section (eieio-object-class section)))
         (thread-id (if (eq section-class 'amacs-thread-section)
                        (let ((value (oref section value)))
                          (if (listp value) (alist-get 'id value) value))
                      (agent-active-thread))))
    (unless thread-id
      (user-error "No thread selected or active"))
    (let ((evidence (read-string "Completion evidence: "))
          (learned (read-string "What was learned: ")))
      (agent-complete-thread thread-id :evidence evidence :learned learned)
      (amacs-hub-refresh)
      (message "Completed thread: %s" thread-id))))

(defun amacs-hub-increase-scratchpad-depth ()
  "Increase scratchpad context depth by 1."
  (interactive)
  (let ((current (or (agent-get 'scratchpad-context-depth) 10)))
    (agent-set 'scratchpad-context-depth (1+ current))
    (amacs-hub-refresh)
    (message "Scratchpad depth: %d" (1+ current))))

(defun amacs-hub-decrease-scratchpad-depth ()
  "Decrease scratchpad context depth by 1 (minimum 0)."
  (interactive)
  (let ((current (or (agent-get 'scratchpad-context-depth) 10)))
    (agent-set 'scratchpad-context-depth (max 0 (1- current)))
    (amacs-hub-refresh)
    (message "Scratchpad depth: %s"
             (if (= (max 0 (1- current)) 0) "all" (max 0 (1- current))))))

(defun amacs-hub-trigger-tick ()
  "Trigger an agent tick (calls `agent-think')."
  (interactive)
  (message "Triggering tick...")
  (agent-think)
  (amacs-hub-refresh))

(defun amacs-hub-trigger-tick-with-thinking ()
  "Trigger tick with extended thinking enabled.
This is a one-shot parameter for deeper reasoning."
  (interactive)
  (message "Triggering tick with extended thinking...")
  ;; For now, same as regular tick - extended thinking would need
  ;; API parameter support which can be added later
  (let ((agent-extended-thinking t))  ; Dynamic binding for future use
    (agent-think))
  (amacs-hub-refresh))

(defun amacs-hub-switch-thread ()
  "Switch to a different thread via completing-read."
  (interactive)
  (let* ((threads (agent-open-threads))
         (thread-names (mapcar (lambda (th)
                                 (cons (or (alist-get 'concern th)
                                           (alist-get 'id th))
                                       (alist-get 'id th)))
                               threads)))
    (if (null thread-names)
        (message "No threads available")
      (let* ((choice (completing-read "Switch to thread: "
                                      (mapcar #'car thread-names) nil t))
             (thread-id (cdr (assoc choice thread-names))))
        (when thread-id
          (agent-switch-thread thread-id)
          (amacs-hub-refresh))))))

(defun amacs-hub-help ()
  "Display hub keybinding help."
  (interactive)
  (with-output-to-temp-buffer "*AMACS Hub Help*"
    (princ "AMACS Hub Keybindings\n")
    (princ "=====================\n\n")
    (princ "Navigation:\n")
    (princ "  TAB     Expand/collapse section inline\n")
    (princ "  RET     Jump to source (buffer, skill, chat tick, etc.)\n")
    (princ "  n/p     Next/previous section\n")
    (princ "  g       Refresh hub\n\n")
    (princ "Actions:\n")
    (princ "  a       Add (thread/buffer/skill based on context)\n")
    (princ "  k       Remove/archive (thread/buffer/skill)\n")
    (princ "  c       Complete thread (with evidence/learned)\n")
    (princ "  s       Switch thread\n\n")
    (princ "Scratchpad:\n")
    (princ "  +       Increase scratchpad depth\n")
    (princ "  -       Decrease scratchpad depth (0 = all)\n\n")
    (princ "Tick:\n")
    (princ "  t       Trigger tick (agent-think)\n")
    (princ "  T       Trigger tick with extended thinking\n\n")
    (princ "API Settings:\n")
    (princ "  M-t     Toggle thinking mode\n")
    (princ "  M-T     Set temperature (0.0-2.0)\n")
    (princ "  M-p     Set top-p (0.0-1.0)\n")
    (princ "  M-m     Set model override\n\n")
    (princ "Other:\n")
    (princ "  ?       Show this help\n")))

;;; API Settings Section (IMP-032)

(defclass amacs-api-settings-section (magit-section) ())

(defun amacs-hub--insert-api-settings ()
  "Insert API settings section showing inference parameters."
  (magit-insert-section (amacs-api-settings-section)
    (magit-insert-heading "API Settings")
    (let* ((settings (agent-api-settings))
           (temp (or (alist-get 'temperature settings) 1.0))
           (top-p (alist-get 'top-p settings))
           (thinking (alist-get 'thinking settings))
           (max-tok (or (alist-get 'max-tokens settings) 8192))
           (model-override (alist-get 'model settings)))
      (insert (format "  Temperature: %.2f" temp))
      (when top-p
        (insert (format " | Top-P: %.2f" top-p)))
      (insert (format " | Thinking: %s" (if thinking "on" "off")))
      (insert (format " | Max Tokens: %d\n" max-tok))
      (when model-override
        (insert (format "  Model Override: %s\n" model-override)))
      (insert "  [M-t toggle thinking | M-T set temp | M-p set top-p | M-m set model]\n")
      (insert "\n"))))

(defun amacs-hub-toggle-thinking ()
  "Toggle extended thinking mode."
  (interactive)
  (let ((current (agent-get-api-param 'thinking)))
    (agent-set-api-param 'thinking (not current))
    (amacs-hub-refresh)
    (message "Thinking: %s" (if (not current) "enabled" "disabled"))))

(defun amacs-hub-set-temperature ()
  "Set temperature parameter."
  (interactive)
  (let* ((current (or (agent-get-api-param 'temperature) 1.0))
         (new-val (read-number (format "Temperature (0.0-2.0, current %.2f): " current) current)))
    (agent-set-api-param 'temperature new-val)
    (amacs-hub-refresh)))

(defun amacs-hub-set-top-p ()
  "Set top-p parameter."
  (interactive)
  (let* ((current (or (agent-get-api-param 'top-p) 1.0))
         (new-val (read-number (format "Top-P (0.0-1.0, current %.2f): " current) current)))
    (agent-set-api-param 'top-p new-val)
    (amacs-hub-refresh)))

(defun amacs-hub-set-model ()
  "Set model override."
  (interactive)
  (let* ((models '("anthropic/claude-sonnet-4"
                   "anthropic/claude-3.5-sonnet"
                   "anthropic/claude-3-haiku"
                   "openai/gpt-4o"
                   "openai/gpt-4o-mini"
                   "google/gemini-pro-1.5"))
         (current (agent-get-api-param 'model))
         (choice (completing-read
                  (format "Model (current: %s): " (or current "default"))
                  (cons "(clear override)" models)
                  nil nil)))
    (if (equal choice "(clear override)")
        (agent-set-api-param 'model nil)
      (agent-set-api-param 'model choice))
    (amacs-hub-refresh)))

;;; Status Section

(defun amacs-hub--insert-status ()
  "Insert status section showing mood, tick, confidence."
  (magit-insert-section (amacs-status-section)
    (magit-insert-heading "Status")
    (let ((tick (agent-current-tick))
          (mood (agent-mood))
          (confidence (agent-confidence))
          (thread (agent-active-thread)))
      (insert (format "  Tick: %d\n" tick))
      (insert (format "  Mood: %s\n" mood))
      (insert (format "  Confidence: %.2f\n" confidence))
      (insert (format "  Active Thread: %s\n" (or thread "none")))
      (insert "\n"))))

;;; Threads Section

(defun amacs-hub--insert-threads ()
  "Insert threads section with active/inactive grouping."
  (magit-insert-section (amacs-threads-section)
    (magit-insert-heading "Threads")
    (let ((active-id (agent-active-thread))
          (open-threads (agent-open-threads)))
      (if (null open-threads)
          (insert "  No threads\n")
        (dolist (thread open-threads)
          (let* ((id (alist-get 'id thread))
                 (concern (alist-get 'concern thread))
                 (is-active (equal id active-id)))
            (magit-insert-section (amacs-thread-section thread is-active)
              (insert (format "  %s %s\n"
                              (if is-active "*" " ")
                              (or concern id)))))))
      (insert "\n"))))

;;; Buffers Section

(defun amacs-hub--insert-buffers ()
  "Insert watched buffers section for current thread."
  (magit-insert-section (amacs-buffers-section)
    (magit-insert-heading "Watched Buffers")
    (let* ((active-id (agent-active-thread))
           (thread (seq-find (lambda (th) (equal (alist-get 'id th) active-id))
                             (agent-open-threads)))
           (buffers (when thread (alist-get 'buffers thread))))
      (if (null buffers)
          (insert "  No buffers\n")
        (dolist (buf buffers)
          (magit-insert-section (amacs-buffer-section buf)
            (insert (format "  %s\n" buf))))))
    (insert "\n")))

;;; Skills Section

(defun amacs-hub--insert-skills ()
  "Insert active skills section."
  (magit-insert-section (amacs-skills-section)
    (magit-insert-heading "Active Skills")
    (let ((skills (agent-active-skills)))
      (if (null skills)
          (insert "  No skills bound\n")
        (dolist (skill skills)
          (let ((name (if (listp skill) (alist-get 'name skill) skill)))
            (magit-insert-section (amacs-skill-section name)
              (insert (format "  %s\n" name)))))))
    (insert "\n")))

;;; Chat Section

(defun amacs-hub--get-chat-buffers ()
  "Return list of buffers with `amacs-chat-mode'."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (bound-and-true-p amacs-chat-mode)))
              (buffer-list)))

(defun amacs-hub--parse-chat-ticks (buffer)
  "Parse tick headings from chat BUFFER.
Returns list of alists with tick, human, agent content."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((ticks '()))
          (while (re-search-forward "^\\* Tick \\([0-9]+\\)" nil t)
            (let* ((tick-num (string-to-number (match-string 1)))
                   (tick-start (match-beginning 0))
                   (tick-end (save-excursion
                               (if (re-search-forward "^\\* Tick " nil t)
                                   (match-beginning 0)
                                 (point-max))))
                   (content (buffer-substring-no-properties tick-start tick-end)))
              (push `((tick . ,tick-num)
                      (content . ,content)
                      (buffer . ,buffer))
                    ticks)))
          (nreverse ticks))))))

(defun amacs-hub--insert-chat ()
  "Insert chat section with expandable ticks."
  (magit-insert-section (amacs-chat-section)
    (magit-insert-heading "Chat")
    (let ((chat-buffers (amacs-hub--get-chat-buffers)))
      (if (null chat-buffers)
          (insert "  No chat buffers\n")
        (dolist (buf chat-buffers)
          (magit-insert-section (amacs-chat-buffer-section buf)
            (insert (format "  %s\n" (buffer-name buf)))
            (let ((ticks (amacs-hub--parse-chat-ticks buf)))
              (dolist (tick-data ticks)
                (let ((tick-num (alist-get 'tick tick-data))
                      (content (alist-get 'content tick-data)))
                  (magit-insert-section (amacs-chat-tick-section tick-data t)
                    (insert (format "    Tick %d\n" tick-num))
                    (magit-insert-heading)
                    (insert content)))))))))
    (insert "\n")))

;;; Monologue Section

(defun amacs-hub--get-recent-monologue-ticks ()
  "Get recent monologue entries with tick info.
Returns list of alists with tick, narrative, diff."
  (let ((monologue-depth (or (agent-get 'monologue-context-depth) 20))
        (entries '()))
    ;; Parse monologue file for recent entries
    (let ((file (expand-file-name "~/.agent/monologue.org")))
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-max))
          (let ((count 0))
            (while (and (< count monologue-depth)
                        (re-search-backward
                         "^\\[\\([^]]+\\)\\]\\[TICK \\([0-9]+\\)\\] \\(.*\\)$"
                         nil t))
              (let* ((timestamp (match-string 1))
                     (tick-num (string-to-number (match-string 2)))
                     (narrative (match-string 3)))
                (push `((tick . ,tick-num)
                        (timestamp . ,timestamp)
                        (narrative . ,narrative))
                      entries))
              (cl-incf count))))))
    entries))

(defun amacs-hub--insert-monologue ()
  "Insert monologue section with expandable tick entries."
  (magit-insert-section (amacs-monologue-section)
    (magit-insert-heading "Monologue")
    (let ((entries (amacs-hub--get-recent-monologue-ticks)))
      (if (null entries)
          (insert "  No monologue entries\n")
        (dolist (entry entries)
          (let* ((tick-num (alist-get 'tick entry))
                 (narrative (alist-get 'narrative entry))
                 (diff (agent-get-tick-diff tick-num))
                 (expanded-content (concat narrative "\n"
                                          (when diff
                                            (concat "\nDiff:\n" diff)))))
            (magit-insert-section (amacs-monologue-tick-section
                                   (cons `(expanded . ,expanded-content) entry)
                                   t)
              (insert (format "  [%d] %s\n"
                              tick-num
                              (truncate-string-to-width narrative 60 nil nil "...")))
              (magit-insert-heading)
              (when expanded-content
                (insert (format "    %s\n" expanded-content))))))))
    (insert "\n")))

;;; Scratchpad Section

(defun amacs-hub--get-scratchpad-buffers ()
  "Return list of buffers with `agent-scratchpad-mode'."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (bound-and-true-p agent-scratchpad-mode)))
              (buffer-list)))

(defun amacs-hub--parse-scratchpad-headings (buffer)
  "Extract org headings from BUFFER for hub display."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((headings '()))
          (while (re-search-forward "^\\(\\*+\\) \\(.+\\)$" nil t)
            (let* ((level (length (match-string 1)))
                   (title (match-string 2))
                   (begin (match-beginning 0))
                   (content-begin (1+ (match-end 0)))
                   (content-end (save-excursion
                                  (if (re-search-forward "^\\*+ " nil t)
                                      (match-beginning 0)
                                    (point-max))))
                   (content (string-trim
                             (buffer-substring-no-properties
                              content-begin content-end))))
              (push `((title . ,title)
                      (level . ,level)
                      (begin . ,begin)
                      (content . ,content)
                      (buffer . ,buffer))
                    headings)))
          (nreverse headings))))))

(defun amacs-hub--insert-scratchpad ()
  "Insert scratchpad section with expandable headings."
  (let ((depth (or (agent-get 'scratchpad-context-depth) 10)))
    (magit-insert-section (amacs-scratchpad-section)
      (magit-insert-heading (format "Scratchpad (depth: %s)"
                                    (if (= depth 0) "all" depth)))
      (let ((scratchpad-buffers (amacs-hub--get-scratchpad-buffers)))
        (if (null scratchpad-buffers)
            (insert "  No scratchpad buffers\n")
          (dolist (buf scratchpad-buffers)
            (magit-insert-section (amacs-scratchpad-buffer-section buf)
              (insert (format "  %s\n" (buffer-name buf)))
              (let ((headings (amacs-hub--parse-scratchpad-headings buf)))
                (dolist (heading headings)
                  (let ((title (alist-get 'title heading))
                        (level (alist-get 'level heading))
                        (content (alist-get 'content heading)))
                    (magit-insert-section (amacs-scratchpad-heading-section heading t)
                      (insert (format "    %s%s\n"
                                      (make-string (* 2 (1- level)) ?\s)
                                      title))
                      (magit-insert-heading)
                      (when (and content (not (string-empty-p content)))
                        (insert (format "      %s\n" content)))))))))))
      (insert "\n"))))

;;; Main Hub Functions

(defun amacs-hub-refresh ()
  "Refresh the hub buffer."
  (interactive)
  (when (eq major-mode 'amacs-hub-mode)
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (magit-insert-section (amacs-hub-section)
        (insert "AMACS Hub\n")
        (insert (make-string 50 ?=))
        (insert "\n\n")
        (amacs-hub--insert-api-settings)
        (amacs-hub--insert-status)
        (amacs-hub--insert-threads)
        (amacs-hub--insert-buffers)
        (amacs-hub--insert-skills)
        (amacs-hub--insert-chat)
        (amacs-hub--insert-monologue)
        (amacs-hub--insert-scratchpad))
      (goto-char (min pos (point-max))))))

;;;###autoload
(defun amacs-hub ()
  "Open the AMACS hub dashboard."
  (interactive)
  (let ((buf (get-buffer-create amacs-hub-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'amacs-hub-mode)
        (amacs-hub-mode))
      (amacs-hub-refresh))
    (switch-to-buffer buf)))

(provide 'amacs-hub)
;;; amacs-hub.el ends here
