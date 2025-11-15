;;; agent-game-provider.el --- AI model provider interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: amacs
;; Keywords: games, agents, ai

;;; Commentary:

;; Generic interface for AI model providers (OpenRouter, Anthropic, OpenAI, Ollama, etc.)
;; Providers implement a simple API: send prompt, get response.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

;;; Provider Registry

(defvar agent-game-providers (make-hash-table :test 'equal)
  "Registry of available AI providers.
Each provider is a plist with:
  :name - Provider name (string)
  :call-fn - Function to call the provider API
  :models - List of available models")

(defun agent-game-provider-register (name call-fn models)
  "Register a provider with NAME, CALL-FN, and available MODELS.
CALL-FN should be a function that takes (model prompt) and returns response text."
  (puthash name
           (list :name name
                 :call-fn call-fn
                 :models models)
           agent-game-providers))

(defun agent-game-provider-get (name)
  "Get provider by NAME."
  (gethash name agent-game-providers))

(defun agent-game-provider-call (provider-name model prompt)
  "Call PROVIDER-NAME with MODEL and PROMPT.
Returns the response text from the model."
  (let* ((provider (agent-game-provider-get provider-name))
         (call-fn (plist-get provider :call-fn)))
    (unless provider
      (error "Unknown provider: %s" provider-name))
    (unless call-fn
      (error "Provider %s has no call function" provider-name))
    (funcall call-fn model prompt)))

;;; HTTP Helpers

(defun agent-game-provider-http-post (url headers body)
  "Make HTTP POST request to URL with HEADERS and BODY.
Returns parsed JSON response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data body)
         (buffer (url-retrieve-synchronously url t nil 30)))
    (unless buffer
      (error "Failed to connect to %s" url))
    (with-current-buffer buffer
      (goto-char (point-min))
      ;; Skip HTTP headers
      (re-search-forward "^$" nil t)
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol))
        (condition-case err
            (json-read)
          (error
           (error "Failed to parse JSON response: %s\nBuffer: %s"
                  (error-message-string err)
                  (buffer-substring-no-properties (point) (point-max)))))))))

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

(defun agent-game-provider-openai-call (model prompt)
  "Call OpenAI-compatible API with MODEL and PROMPT."
  (unless agent-game-openai-api-key
    (error "agent-game-openai-api-key not set. Configure API key first."))

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
                  (max_tokens . 500))))
         (response (agent-game-provider-http-post url headers body)))

    ;; Extract response text from OpenAI format
    (let* ((choices (alist-get 'choices response))
           (first-choice (aref choices 0))
           (message (alist-get 'message first-choice))
           (content (alist-get 'content message)))
      (unless content
        (error "No content in API response: %S" response))
      content)))

;; Register OpenRouter provider
(agent-game-provider-register
 "openrouter"
 #'agent-game-provider-openai-call
 '("anthropic/claude-3.5-sonnet"
   "openai/gpt-4o"
   "google/gemini-pro"
   "meta-llama/llama-3.1-70b-instruct"
   "qwen/qwen-2.5-72b-instruct"))

;; Register OpenAI provider (same implementation, different base URL)
(defun agent-game-provider-openai-native-call (model prompt)
  "Call native OpenAI API with MODEL and PROMPT."
  (let ((agent-game-openai-base-url "https://api.openai.com/v1"))
    (agent-game-provider-openai-call model prompt)))

(agent-game-provider-register
 "openai"
 #'agent-game-provider-openai-native-call
 '("gpt-4o" "gpt-4o-mini" "gpt-4-turbo" "gpt-3.5-turbo"))

;; Register Ollama provider (local)
(defun agent-game-provider-ollama-call (model prompt)
  "Call local Ollama API with MODEL and PROMPT."
  (let ((agent-game-openai-base-url "http://localhost:11434/v1")
        (agent-game-openai-api-key "ollama")) ; Ollama doesn't need real key
    (agent-game-provider-openai-call model prompt)))

(agent-game-provider-register
 "ollama"
 #'agent-game-provider-ollama-call
 '("granite3-dense:8b" "qwen2.5:14b" "llama3.1:8b" "mistral:latest"))

(provide 'agent-game-provider)
;;; agent-game-provider.el ends here
