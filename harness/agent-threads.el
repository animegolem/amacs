;;; agent-threads.el --- Thread-centric work management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Threads are the unit of work ownership. Each thread owns:
;; - A concern (what problem am I solving?)
;; - Buffers (what files matter for this work?)
;; - Skill tags (what capabilities apply?)
;; - Hydration state (is this the active focus?)
;;
;; Only the active thread's buffers are loaded into context.
;; Pending threads exist as metadata summaries.
;;
;; Threads use alist format with symbol keys for consistency with
;; consciousness and clean alist-get access patterns.
;;
;; See: AI-ADR-001-thread-centric-context
;;      amacs-rfc-v3.md Part 5-6
;;      STYLE.md (alist conventions)

;;; Code:

(require 'cl-lib)
(require 'agent-consciousness)

;;; Thread ID Generation

(defvar agent--thread-counter 0
  "Counter for generating unique thread IDs.")

(defun agent--generate-thread-id (concern)
  "Generate a unique thread ID based on CONCERN."
  (let* ((base (downcase (replace-regexp-in-string 
                          "[^a-z0-9]+" "-" 
                          (substring concern 0 (min 30 (length concern))))))
         (base (string-trim base "-")))
    (cl-incf agent--thread-counter)
    (format "%s-%d" base agent--thread-counter)))

;;; Skill Tag Inference

(defun agent-infer-skill-tags (_buffers primary-mode)
  "Infer skill tags from _BUFFERS and PRIMARY-MODE.
Returns a list of skill tag strings.
_BUFFERS is reserved for future buffer-pattern-based tags."
  (let ((tags '()))
    ;; Add mode-based tag
    (when primary-mode
      (let ((mode-name (symbol-name primary-mode)))
        (push (replace-regexp-in-string "-mode$" "" mode-name) tags)))
    ;; Could add project-based tags here in future
    ;; Could add buffer-pattern-based tags here
    (delete-dups (nreverse tags))))

;;; Thread Creation

(defun agent-create-thread (concern &optional buffers)
  "Create a new thread for CONCERN with optional initial BUFFERS.
If BUFFERS is nil, uses current buffer.
Returns the new thread alist (not yet added to consciousness)."
  (let* ((bufs (or buffers
                   (when (buffer-name)
                     (list (buffer-name)))))
         (primary-buf (car bufs))
         (mode (when primary-buf
                 (if (get-buffer primary-buf)
                     (buffer-local-value 'major-mode (get-buffer primary-buf))
                   'fundamental-mode)))
         (tags (agent-infer-skill-tags bufs mode))
         (thread-id (agent--generate-thread-id concern)))
    ;; Return alist with symbol keys
    ;; Use explicit list/cons to avoid backquote structure sharing
    ;; (setf on alist-get mutates cons cells)
    (list (cons 'id thread-id)
          (cons 'started-tick (agent-current-tick))
          (cons 'concern concern)
          (cons 'goal nil)
          (cons 'deliverable nil)
          (cons 'bound-skills nil)
          (cons 'thread-type 'exploratory)
          (cons 'buffers bufs)
          (cons 'primary-mode mode)
          (cons 'skill-tags tags)
          (cons 'hydrated nil)
          (cons 'priority 2)
          (cons 'approach nil)
          (cons 'blocking nil)
          (cons 'completion-tick nil)
          (cons 'completion-evidence nil)
          (cons 'learned nil)
          (cons 'active-loras nil))))

;;; Thread Accessors

(defun agent-get-thread (thread-id)
  "Get thread by THREAD-ID from open-threads or completed-threads."
  (or (cl-find thread-id (agent-get 'open-threads)
               :key (lambda (thr) (alist-get 'id thr))
               :test #'equal)
      (cl-find thread-id (agent-get 'completed-threads)
               :key (lambda (thr) (alist-get 'id thr))
               :test #'equal)))

(defun agent-get-active-thread ()
  "Return the currently active thread alist, or nil."
  (let ((active-id (agent-get 'active-thread)))
    (when active-id
      (agent-get-thread active-id))))

(defun agent-get-pending-threads ()
  "Return list of open threads that are not the active thread."
  (let ((active-id (agent-get 'active-thread)))
    (seq-filter (lambda (thr)
                  (not (equal (alist-get 'id thr) active-id)))
                (agent-get 'open-threads))))

;;; Thread Mutation Helpers

(defun agent--update-thread (thread-id updates)
  "Apply UPDATES alist to thread with THREAD-ID.
UPDATES is an alist of (key . value) pairs to set."
  (let ((threads (agent-get 'open-threads)))
    (agent-set 'open-threads
               (mapcar (lambda (thr)
                         (if (equal (alist-get 'id thr) thread-id)
                             (progn
                               (dolist (pair updates)
                                 (setf (alist-get (car pair) thr) (cdr pair)))
                               thr)
                           thr))
                       threads))))

;;; Thread Lifecycle

(defun agent-add-thread (thread &optional activate)
  "Add THREAD to open-threads. If ACTIVATE is non-nil, make it active."
  (let ((threads (agent-get 'open-threads)))
    (agent-set 'open-threads (cons thread threads)))
  (when activate
    (agent-switch-thread (alist-get 'id thread)))
  thread)

(defun agent-switch-thread (thread-id)
  "Switch active thread to THREAD-ID.
Dehydrates the old active thread, hydrates the new one."
  (let ((old-id (agent-get 'active-thread)))
    ;; Dehydrate old thread
    (when old-id
      (agent--update-thread old-id '((hydrated . nil))))
    ;; Set new active
    (agent-set 'active-thread thread-id)
    ;; Hydrate new thread
    (when thread-id
      (agent--update-thread thread-id '((hydrated . t))))
    (message "Switched to thread: %s" thread-id)
    thread-id))

(defun agent-complete-thread (thread-id &rest args)
  "Mark THREAD-ID as completed with completion data.
ARGS is a plist with :evidence and :learned keys."
  (let* ((evidence (plist-get args :evidence))
         (learned (plist-get args :learned))
         (thread (agent-get-thread thread-id))
         (open-threads (agent-get 'open-threads))
         (completed-threads (agent-get 'completed-threads)))
    (when thread
      ;; Update completion fields using setf on alist-get
      (setf (alist-get 'completion-tick thread) (agent-current-tick))
      (setf (alist-get 'completion-evidence thread) evidence)
      (setf (alist-get 'learned thread) learned)
      (setf (alist-get 'hydrated thread) nil)

      ;; Move from open to completed
      (agent-set 'open-threads
                 (seq-filter (lambda (thr)
                               (not (equal (alist-get 'id thr) thread-id)))
                             open-threads))
      (agent-set 'completed-threads (cons thread completed-threads))

      ;; If this was active, clear active
      (when (equal (agent-get 'active-thread) thread-id)
        (agent-set 'active-thread nil))

      (message "Completed thread: %s (learned: %s)" thread-id learned)
      thread)))

(defun agent-archive-thread (thread-id)
  "Archive THREAD-ID without completion data.
Used to remove threads that are no longer relevant.
Unlike complete, this doesn't prompt for evidence/learned."
  (let* ((thread (agent-get-thread thread-id))
         (open-threads (agent-get 'open-threads))
         (completed-threads (agent-get 'completed-threads)))
    (when thread
      ;; Mark as archived (not completed)
      (setf (alist-get 'archived thread) t)
      (setf (alist-get 'archived-tick thread) (agent-current-tick))
      (setf (alist-get 'hydrated thread) nil)

      ;; Move from open to completed (archived threads go here too)
      (agent-set 'open-threads
                 (seq-filter (lambda (thr)
                               (not (equal (alist-get 'id thr) thread-id)))
                             open-threads))
      (agent-set 'completed-threads (cons thread completed-threads))

      ;; If this was active, clear active
      (when (equal (agent-get 'active-thread) thread-id)
        (agent-set 'active-thread nil))

      (message "Archived thread: %s" thread-id)
      thread)))

;;; Thread Summaries (for dehydrated context)

(defun agent-thread-summary (thread)
  "Return a dehydrated summary of THREAD for context inclusion.
Includes metadata but not buffer contents. Returns alist."
  (when thread
    `((id . ,(alist-get 'id thread))
      (concern . ,(alist-get 'concern thread))
      (approach . ,(alist-get 'approach thread))
      (started-tick . ,(alist-get 'started-tick thread))
      (buffers . ,(alist-get 'buffers thread))
      (priority . ,(alist-get 'priority thread)))))

;;; Thread Modification

(defun agent-thread-set-approach (thread-id approach)
  "Set the approach for THREAD-ID."
  (agent--update-thread thread-id `((approach . ,approach))))

(defun agent-thread-set-goal (thread-id goal &optional deliverable)
  "Set the goal and optional DELIVERABLE for THREAD-ID.
Also sets thread-type to deliverable if deliverable is provided."
  (let ((updates `((goal . ,goal))))
    (when deliverable
      (setq updates (append updates `((deliverable . ,deliverable)
                                      (thread-type . deliverable)))))
    (agent--update-thread thread-id updates)))

(defun agent-thread-add-buffer (thread-id buffer-name)
  "Add BUFFER-NAME to THREAD-ID's buffer list."
  (let* ((thread (agent-get-thread thread-id))
         (buffers (alist-get 'buffers thread)))
    (unless (member buffer-name buffers)
      (agent--update-thread thread-id
                            `((buffers . ,(cons buffer-name buffers)))))))

(defun agent-thread-remove-buffer (thread-id buffer-name)
  "Remove BUFFER-NAME from THREAD-ID's buffer list."
  (let* ((thread (agent-get-thread thread-id))
         (buffers (alist-get 'buffers thread)))
    (agent--update-thread thread-id
                          `((buffers . ,(remove buffer-name buffers))))))

;;; Initialization Helper

(defun agent-ensure-default-thread ()
  "Ensure at least one thread exists. Creates default if needed.
Called during cold start."
  (when (null (agent-get 'open-threads))
    (let ((default-thread (agent-create-thread "Initial exploration"
                                               '("*scratch*"))))
      (agent-add-thread default-thread t)
      (message "Created default thread: %s" (alist-get 'id default-thread)))))

;;; Inspection

(defun agent-threads-info ()
  "Display thread information."
  (interactive)
  (with-output-to-temp-buffer "*Agent Threads*"
    (princ "AMACS Threads\n")
    (princ "=============\n\n")
    (princ (format "Active thread: %s\n\n" (or (agent-get 'active-thread) "none")))

    (princ "Open Threads:\n")
    (let ((open (agent-get 'open-threads)))
      (if open
          (dolist (thr open)
            (princ (format "  [%s] %s\n"
                           (alist-get 'id thr)
                           (alist-get 'concern thr)))
            (princ (format "    buffers: %s\n" (alist-get 'buffers thr)))
            (princ (format "    hydrated: %s\n" (alist-get 'hydrated thr))))
        (princ "  (none)\n")))

    (princ "\nCompleted Threads:\n")
    (let ((completed (agent-get 'completed-threads)))
      (if completed
          (dolist (thr (seq-take completed 5))
            (princ (format "  [%s] %s\n"
                           (alist-get 'id thr)
                           (alist-get 'concern thr)))
            (princ (format "    learned: %s\n"
                           (or (alist-get 'learned thr) "(none)"))))
        (princ "  (none)\n")))))

(provide 'agent-threads)
;;; agent-threads.el ends here
