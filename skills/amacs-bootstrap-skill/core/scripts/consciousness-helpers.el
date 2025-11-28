;;; consciousness-helpers.el --- Utilities for consciousness management -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper functions for reading, modifying, and managing the agent-consciousness
;; variable. These provide convenient abstractions over the raw plist operations.

;;; Code:

(defvar agent-consciousness nil
  "The agent's working memory. See references/consciousness-schema.md for full structure.")

(defvar agent-consciousness-file "~/.agent/consciousness.el"
  "File where consciousness is persisted between sessions.")

;;; Persistence

(defun agent-persist-consciousness ()
  "Write consciousness to disk for crash recovery.
Called automatically at end of each tick."
  (with-temp-file agent-consciousness-file
    (insert ";;; Agent consciousness - auto-generated\n")
    (insert (format "(setq agent-consciousness '%S)\n" agent-consciousness))))

(defun agent-load-consciousness ()
  "Load consciousness from disk.
Called on warm start."
  (when (file-exists-p agent-consciousness-file)
    (load agent-consciousness-file t t)
    (message "Loaded consciousness from %s" agent-consciousness-file)))

;;; Basic Accessors

(defun agent-get (key)
  "Get KEY from consciousness."
  (plist-get agent-consciousness key))

(defun agent-set (key value)
  "Set KEY to VALUE in consciousness."
  (setq agent-consciousness (plist-put agent-consciousness key value)))

;;; Identity & Temporal

(defun agent-identity ()
  "Return the agent's identity string."
  (agent-get :identity))

(defun agent-current-tick ()
  "Return the current tick number."
  (agent-get :current-tick))

(defun agent-increment-tick ()
  "Increment the tick counter and update timestamp."
  (agent-set :current-tick (1+ (or (agent-current-tick) 0)))
  (let ((now (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
    (agent-set :last-inference-time (agent-get :current-time))
    (agent-set :current-time now)))

(defun agent-check-gap ()
  "Check if there's been a long gap since last inference.
Sets :long-gap-detected if gap > 1 hour."
  (let* ((last (agent-get :last-inference-time))
         (now (agent-get :current-time)))
    (when (and last now)
      (let* ((last-time (parse-iso8601-time-string last))
             (now-time (parse-iso8601-time-string now))
             (diff (float-time (time-subtract now-time last-time))))
        (agent-set :long-gap-detected (> diff 3600))))))

;;; Confidence

(defun agent-confidence ()
  "Return current confidence level (0.0-1.0)."
  (or (agent-get :confidence) 0.5))

(defun agent-set-confidence (value)
  "Set confidence to VALUE (clamped to 0.0-1.0)."
  (agent-set :confidence (max 0.0 (min 1.0 value))))

(defun agent-record-action (action-name confidence)
  "Record an action with its confidence score.
Maintains a rolling window of recent actions."
  (let* ((current-tick (agent-current-tick))
         (action-record `(:tick ,current-tick :action ,action-name :confidence ,confidence))
         (last-actions (agent-get :last-actions))
         (updated (cons action-record (seq-take last-actions 19)))) ; Keep last 20
    (agent-set :last-actions updated)
    (agent-set-confidence confidence)))

(defun agent-confidence-trend ()
  "Return the confidence trend: :declining, :stable, or :improving.
Based on last 5 actions."
  (let* ((actions (seq-take (agent-get :last-actions) 5))
         (confidences (mapcar (lambda (a) (plist-get a :confidence)) actions)))
    (if (< (length confidences) 3)
        :insufficient-data
      (let ((first-half (seq-take confidences 2))
            (second-half (seq-drop confidences 2)))
        (cond
         ((> (- (apply #'+ first-half) (apply #'+ second-half)) 0.3) :declining)
         ((< (- (apply #'+ first-half) (apply #'+ second-half)) -0.3) :improving)
         (t :stable))))))

;;; Mood

(defun agent-mood ()
  "Return current mood keyword."
  (or (agent-get :mood) :neutral))

(defun agent-set-mood (mood)
  "Set mood to MOOD (keyword like :focused, :curious, :stressed)."
  (agent-set :mood mood))

;;; Thread Management

(defun agent-active-thread ()
  "Return the ID of the currently active thread."
  (agent-get :active-thread))

(defun agent-set-active-thread (thread-id)
  "Set THREAD-ID as the active thread."
  (agent-set :active-thread thread-id))

(defun agent-open-threads ()
  "Return list of open threads."
  (agent-get :open-threads))

(defun agent-get-thread (thread-id)
  "Return the thread with THREAD-ID, or nil."
  (seq-find (lambda (t) (equal (plist-get t :id) thread-id))
            (agent-open-threads)))

(defun agent-create-thread (id concern &optional buffers approach)
  "Create a new thread with ID and CONCERN.
Optional BUFFERS and APPROACH."
  (let* ((current-threads (agent-open-threads))
         (budget (or (agent-get :thread-budget) 3)))
    (when (>= (length current-threads) budget)
      (error "Thread budget exhausted. Consolidate before creating new threads."))
    (let ((new-thread `(:id ,id
                        :started-tick ,(agent-current-tick)
                        :priority 2
                        :concern ,concern
                        :buffers ,(or buffers '())
                        :approach ,(or approach "")
                        :blocking nil)))
      (agent-set :open-threads (cons new-thread current-threads))
      (agent-set :active-thread id)
      new-thread)))

(defun agent-update-thread (thread-id &rest props)
  "Update THREAD-ID with PROPS (plist of properties to set)."
  (let ((threads (agent-open-threads)))
    (agent-set :open-threads
               (mapcar (lambda (t)
                         (if (equal (plist-get t :id) thread-id)
                             (let ((updated t))
                               (while props
                                 (setq updated (plist-put updated (car props) (cadr props)))
                                 (setq props (cddr props)))
                               updated)
                           t))
                       threads))))

(defun agent-complete-thread (thread-id outcome learned)
  "Complete THREAD-ID with OUTCOME description and LEARNED insights."
  (let* ((thread (agent-get-thread thread-id))
         (completed `(:id ,thread-id
                      :completed-tick ,(agent-current-tick)
                      :outcome ,outcome
                      :learned ,learned))
         (completed-threads (agent-get :completed-threads)))
    ;; Remove from open threads
    (agent-set :open-threads
               (seq-filter (lambda (t) (not (equal (plist-get t :id) thread-id)))
                           (agent-open-threads)))
    ;; Add to completed (keep last 10)
    (agent-set :completed-threads
               (cons completed (seq-take completed-threads 9)))
    ;; Clear active thread if it was this one
    (when (equal (agent-active-thread) thread-id)
      (let ((remaining (agent-open-threads)))
        (agent-set :active-thread (when remaining (plist-get (car remaining) :id)))))
    completed))

;;; Monologue

(defvar agent-monologue-file "~/.agent/monologue.org"
  "File where monologue is appended.")

(defvar agent-monologue-window-size 100
  "Number of recent monologue lines to keep in consciousness.")

(defun agent-append-monologue (line)
  "Add LINE to permanent monologue log and rolling window in consciousness."
  (let ((timestamped (format "[%s][TICK %s] %s"
                             (format-time-string "%Y-%m-%d %H:%M")
                             (agent-current-tick)
                             line)))
    ;; Append to file
    (append-to-file (concat timestamped "\n") nil agent-monologue-file)
    ;; Update rolling window
    (let* ((recent (agent-get :recent-monologue))
           (updated (cons line (seq-take recent (1- agent-monologue-window-size)))))
      (agent-set :recent-monologue updated))))

(defun agent-recent-monologue (&optional n)
  "Return the last N monologue entries (default: 10)."
  (seq-take (agent-get :recent-monologue) (or n 10)))

;;; Human Review

(defun agent-request-human-review (reason)
  "Request human review with REASON."
  (agent-set :human-review-requested
             `(:requested t
               :reason ,reason
               :requested-at-tick ,(agent-current-tick)))
  (agent-append-monologue (format "HUMAN REVIEW REQUESTED: %s" reason)))

(defun agent-clear-human-review ()
  "Clear the human review request."
  (agent-set :human-review-requested nil))

(defun agent-human-review-pending-p ()
  "Return t if a human review is pending."
  (plist-get (agent-get :human-review-requested) :requested))

;;; Budget

(defun agent-budget-pressure ()
  "Return current budget pressure: :low, :moderate, :high, :critical."
  (or (plist-get (agent-get :budget) :pressure) :unknown))

(defun agent-record-cost (cost)
  "Record a cost of COST to the budget tracker."
  (let* ((budget (agent-get :budget))
         (current (or (plist-get budget :cost-so-far) 0))
         (limit (or (plist-get budget :budget-limit) 5.0))
         (count (or (plist-get budget :inference-count) 0))
         (new-total (+ current cost))
         (ratio (/ new-total limit))
         (pressure (cond ((< ratio 0.5) :low)
                         ((< ratio 0.75) :moderate)
                         ((< ratio 0.9) :high)
                         (t :critical))))
    (agent-set :budget
               `(:cost-so-far ,new-total
                 :budget-limit ,limit
                 :inference-count ,(1+ count)
                 :avg-cost ,(/ new-total (1+ count))
                 :pressure ,pressure))
    (when (eq pressure :critical)
      (agent-request-human-review "Budget critical - approaching limit"))))

;;; Skills

(defun agent-active-skills ()
  "Return list of currently active skills."
  (agent-get :active-skills))

(defun agent-record-skill-use (skill-name)
  "Record that SKILL-NAME was used."
  (let* ((skills (agent-active-skills))
         (existing (seq-find (lambda (s) (equal (plist-get s :name) skill-name)) skills)))
    (if existing
        ;; Update existing
        (agent-set :active-skills
                   (mapcar (lambda (s)
                             (if (equal (plist-get s :name) skill-name)
                                 (plist-put s :uses (1+ (or (plist-get s :uses) 0)))
                               s))
                           skills))
      ;; Add new
      (agent-set :active-skills
                 (cons `(:name ,skill-name
                         :loaded-at-tick ,(agent-current-tick)
                         :uses 1)
                       skills)))))

;;; Initialization

(defun agent-init-consciousness (identity)
  "Initialize a fresh consciousness with IDENTITY."
  (setq agent-consciousness
        `(:identity ,identity
          :current-tick 0
          :current-time ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
          :last-inference-time nil
          :long-gap-detected nil
          :mood :awakening
          :confidence 0.5
          :active-thread nil
          :thread-budget 3
          :open-threads ()
          :completed-threads ()
          :last-actions ()
          :watching-buffers ()
          :focus nil
          :last-commit nil
          :recent-monologue ("Initializing consciousness...")
          :active-skills ()
          :human-review-requested nil
          :budget (:cost-so-far 0 :budget-limit 5.0 :inference-count 0 :pressure :low)
          :cognitive-mode nil))
  (agent-persist-consciousness)
  agent-consciousness)

(provide 'consciousness-helpers)
;;; consciousness-helpers.el ends here
