;;; agent-game-provider.el --- AI model provider interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents, ai

;;; Commentary:

;; Generic interface for AI model providers (OpenRouter, Anthropic, OpenAI, Ollama, etc.)
;; Providers implement async API calls to avoid blocking Emacs.
;;
;; Requires: plz library (available on MELPA)
;;   M-x package-install RET plz RET

;;; Code:

(require 'json)
(require 'cl-lib)

;; Try to load plz, fall back to sync if not available
(defvar agent-game-provider-use-async t
  "If non-nil and plz is available, use async HTTP calls.")

(condition-case nil
    (require 'plz)
  (error
   (setq agent-game-provider-use-async nil)
   (message "plz not found - using synchronous HTTP (will block Emacs)")))

;;; Provider Registry

(defvar agent-game-providers (make-hash-table :test 'equal)
  "Registry of available AI providers.
Each provider is a plist with:
  :name - Provider name (string)
  :call-fn - Function to call the provider API (async)
  :models - List of available models")

(defun agent-game-provider-register (name call-fn models)
  "Register a provider with NAME, CALL-FN, and available MODELS.
CALL-FN should be a function that takes (model prompt on-success on-error)."
  (puthash name
           (list :name name
                 :call-fn call-fn
                 :models models)
           agent-game-providers))

(defun agent-game-provider-get (name)
  "Get provider by NAME."
  (gethash name agent-game-providers))

(defun agent-game-provider-call-async (provider-name model prompt on-success on-error)
  "Call PROVIDER-NAME with MODEL and PROMPT asynchronously.
ON-SUCCESS is called with the response text.
ON-ERROR is called with error info."
  (let* ((provider (agent-game-provider-get provider-name))
         (call-fn (plist-get provider :call-fn)))
    (unless provider
      (funcall on-error (format "Unknown provider: %s" provider-name))
      (cl-return-from agent-game-provider-call-async nil))
    (unless call-fn
      (funcall on-error (format "Provider %s has no call function" provider-name))
      (cl-return-from agent-game-provider-call-async nil))
    (funcall call-fn model prompt on-success on-error)))

;; Synchronous wrapper for simple cases
(defun agent-game-provider-call (provider-name model prompt)
  "Call PROVIDER-NAME with MODEL and PROMPT synchronously.
Returns the response text. Blocks Emacs - prefer async version."
  (let ((result nil)
        (error-msg nil)
        (done nil))
    (agent-game-provider-call-async
     provider-name model prompt
     (lambda (response) (setq result response done t))
     (lambda (err) (setq error-msg err done t)))
    ;; Block until done (for backwards compatibility)
    (while (not done)
      (sleep-for 0.1))
    (if error-msg
        (error "%s" error-msg)
      result)))

;;; OpenAI-Compatible API (OpenRouter, OpenAI, local APIs)

(defvar agent-game-openai-api-key nil
  "API key for OpenAI-compatible providers.
Set this or use agent-game-config.el to configure.")

(defvar agent-game-openai-base-url "https://openrouter.ai/api/v1"
  "Base URL for OpenAI-compatible API.
Default is OpenRouter. Change to:
  - https://api.openai.com/v1 for OpenAI
  - http://localhost:11434/v1 for Ollama
  - Other compatible endpoints")

(defvar agent-game-openai-site-url "https://github.com/animegolem/amacs"
  "Site URL for OpenRouter (optional, for rankings).")

(defvar agent-game-openai-app-name "Agent-Games-Framework"
  "App name for OpenRouter (optional, for rankings).")

(defun agent-game-provider-openai-call-async (model prompt on-success on-error)
  "Call OpenAI-compatible API with MODEL and PROMPT asynchronously."
  (unless agent-game-openai-api-key
    (funcall on-error "agent-game-openai-api-key not set. Configure API key first.")
    (cl-return-from agent-game-provider-openai-call-async nil))

  (let* ((url (concat agent-game-openai-base-url "/chat/completions"))
         (headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(format "Bearer %s" agent-game-openai-api-key))
                   ("HTTP-Referer" . ,agent-game-openai-site-url)
                   ("X-Title" . ,agent-game-openai-app-name)))
         (body (json-encode
                `((model . ,model)
                  (messages . [((role . "user")
                               (content . ,prompt))])
                  (temperature . 0.7)
                  (max_tokens . 500)))))

    (if (and agent-game-provider-use-async (featurep 'plz))
        ;; Async path using plz
        (plz 'post url
          :headers headers
          :body body
          :as 'json-read
          :then (lambda (response)
                  (condition-case err
                      (let* ((choices (alist-get 'choices response))
                             (first-choice (aref choices 0))
                             (message (alist-get 'message first-choice))
                             (content (alist-get 'content message)))
                        (if content
                            (funcall on-success content)
                          (funcall on-error (format "No content in response: %S" response))))
                    (error
                     (funcall on-error (format "Parse error: %s" (error-message-string err))))))
          :else (lambda (err)
                  (funcall on-error (format "HTTP error: %s" err))))

      ;; Sync fallback using url-retrieve
      (condition-case err
          (let* ((url-request-method "POST")
                 (url-request-extra-headers headers)
                 (url-request-data body)
                 (buffer (url-retrieve-synchronously url t nil 30)))
            (unless buffer
              (funcall on-error (format "Failed to connect to %s" url))
              (cl-return-from agent-game-provider-openai-call-async nil))
            (with-current-buffer buffer
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'symbol)
                     (response (json-read))
                     (choices (alist-get 'choices response))
                     (first-choice (aref choices 0))
                     (message (alist-get 'message first-choice))
                     (content (alist-get 'content message)))
                (kill-buffer buffer)
                (if content
                    (funcall on-success content)
                  (funcall on-error (format "No content in response: %S" response))))))
        (error
         (funcall on-error (format "Request error: %s" (error-message-string err))))))))

;; Register OpenRouter provider
(agent-game-provider-register
 "openrouter"
 #'agent-game-provider-openai-call-async
 '("anthropic/claude-3.5-sonnet"
   "openai/gpt-4o"
   "google/gemini-pro"
   "meta-llama/llama-3.1-70b-instruct"
   "qwen/qwen-2.5-72b-instruct"))

;; Register OpenAI provider
(defun agent-game-provider-openai-native-call-async (model prompt on-success on-error)
  "Call native OpenAI API with MODEL and PROMPT asynchronously."
  (let ((agent-game-openai-base-url "https://api.openai.com/v1"))
    (agent-game-provider-openai-call-async model prompt on-success on-error)))

(agent-game-provider-register
 "openai"
 #'agent-game-provider-openai-native-call-async
 '("gpt-4o" "gpt-4o-mini" "gpt-4-turbo" "gpt-3.5-turbo"))

;; Register Ollama provider (local)
(defun agent-game-provider-ollama-call-async (model prompt on-success on-error)
  "Call local Ollama API with MODEL and PROMPT asynchronously."
  (let ((agent-game-openai-base-url "http://localhost:11434/v1")
        (agent-game-openai-api-key "ollama"))
    (agent-game-provider-openai-call-async model prompt on-success on-error)))

(agent-game-provider-register
 "ollama"
 #'agent-game-provider-ollama-call-async
 '("granite3-dense:8b" "qwen2.5:14b" "llama3.1:8b" "mistral:latest"))

(provide 'agent-game-provider)
;;; agent-game-provider.el ends here
