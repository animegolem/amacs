;;; agent-api.el --- OpenAI-compatible API client -*- lexical-binding: t; -*-

;; Copyright (C) 2025 AMACS Project
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; HTTP client for OpenAI-compatible APIs (OpenRouter, OpenAI, etc).
;; Uses built-in url.el - no external dependencies.
;;
;; Configuration via ~/.agent/config.el:
;;   (setq agent-api-key "sk-or-...")
;;   (setq agent-api-endpoint "https://openrouter.ai/api/v1/chat/completions")
;;   (setq agent-model "anthropic/claude-3.5-sonnet")
;;
;; See: AI-IMP-011-first-inference

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)

;;; Configuration Variables

(defvar agent-api-key nil
  "API key for OpenAI-compatible endpoint.
Set via OPENROUTER_API_KEY env var (preferred) or config file.")

(defvar agent-api-endpoint "https://openrouter.ai/api/v1/chat/completions"
  "API endpoint URL. OpenRouter by default.
Can override via OPENROUTER_API_ENDPOINT env var or config file.")

(defvar agent-model "anthropic/claude-3.5-sonnet"
  "Model identifier to use for inference.
Can override via OPENROUTER_MODEL env var or config file.")

(defvar agent-api-timeout 60
  "Timeout in seconds for API calls.")

(defvar agent-api-max-tokens 1024
  "Maximum tokens in response.")

;;; Config Loading

(defun agent-load-config ()
  "Load API configuration from environment variables and/or config file.
Priority: env vars > config file > defaults.
Returns t if API key is available."
  ;; First, try config file (lower priority, can be overridden)
  (let ((config-file (expand-file-name "~/.agent/config.el")))
    (when (file-exists-p config-file)
      (load config-file t t)
      (message "Loaded agent config from %s" config-file)))

  ;; Then, env vars override (higher priority)
  (when-let* ((env-key (getenv "OPENROUTER_API_KEY")))
    (setq agent-api-key env-key)
    (message "Using API key from OPENROUTER_API_KEY env var"))

  (when-let* ((env-endpoint (getenv "OPENROUTER_API_ENDPOINT")))
    (setq agent-api-endpoint env-endpoint))

  (when-let* ((env-model (getenv "OPENROUTER_MODEL")))
    (setq agent-model env-model))

  (agent-api-configured-p))

(defun agent-api-configured-p ()
  "Return t if API is properly configured."
  (and agent-api-key
       (stringp agent-api-key)
       (> (length agent-api-key) 0)))

;;; JSON Helpers

(defun agent--json-read-from-string (string)
  "Parse JSON STRING, returning nil on error."
  (condition-case err
      (json-read-from-string string)
    (error
     (message "JSON parse error: %s" (error-message-string err))
     nil)))

(defun agent--json-encode (object)
  "Encode OBJECT to JSON string."
  (let ((json-encoding-pretty-print nil))
    (json-encode object)))

;;; HTTP Request

(defun agent-api-call (messages &optional temperature)
  "Call the API with MESSAGES array and optional TEMPERATURE.
MESSAGES should be a list of alists with role and content keys.

Returns a plist:
  :content  - The assistant's response text (or nil on error)
  :usage    - Token usage plist (:prompt :completion :total)
  :model    - Model that was used
  :error    - Error message if call failed
  :raw      - Raw response for debugging"

  ;; Check configuration first
  (if (not (agent-api-configured-p))
      '(:content nil :error "API key not configured. Set OPENROUTER_API_KEY env var or create ~/.agent/config.el")

    (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " agent-api-key))
            ("HTTP-Referer" . "https://github.com/anthropics/amacs")
            ("X-Title" . "AMACS Agent")))
         (request-body
          (agent--json-encode
           `((model . ,agent-model)
             (messages . ,(vconcat messages))
             (max_tokens . ,agent-api-max-tokens)
             (temperature . ,(or temperature 0.7)))))
         ;; Must encode as UTF-8 and force unibyte to avoid "Multibyte text in HTTP request"
         (url-request-data (encode-coding-string request-body 'utf-8 t))
         (url-show-status nil)
         response-buffer
         response-body
         parsed)

    ;; Make the request with timeout
    (condition-case err
        (with-timeout (agent-api-timeout
                       (list :content nil :error "Request timed out"))
          (setq response-buffer
                (url-retrieve-synchronously agent-api-endpoint t t agent-api-timeout))

          (if (not response-buffer)
              (list :content nil :error "No response from server")

            ;; Extract response body
            (with-current-buffer response-buffer
              (goto-char (point-min))
              ;; Skip HTTP headers
              (re-search-forward "^\r?\n" nil t)
              (setq response-body (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer response-buffer)

            ;; Parse JSON
            (setq parsed (agent--json-read-from-string response-body))

            (if (not parsed)
                (list :content nil
                      :error "Failed to parse response"
                      :raw response-body)

              ;; Check for API error
              (if-let* ((api-error (alist-get 'error parsed)))
                  (list :content nil
                        :error (or (alist-get 'message api-error)
                                   (format "%s" api-error))
                        :raw parsed)

                ;; Extract successful response
                (let* ((choices (alist-get 'choices parsed))
                       (first-choice (and choices (> (length choices) 0) (aref choices 0)))
                       (message (alist-get 'message first-choice))
                       (content (alist-get 'content message))
                       (usage (alist-get 'usage parsed))
                       (model-used (alist-get 'model parsed)))

                  (list :content content
                        :usage (when usage
                                 (list :prompt (alist-get 'prompt_tokens usage)
                                       :completion (alist-get 'completion_tokens usage)
                                       :total (alist-get 'total_tokens usage)))
                        :model model-used
                        :error nil
                        :raw parsed))))))

      ;; Handle errors
      (error
       (list :content nil
             :error (format "Request failed: %s" (error-message-string err))))))))

;;; Cost Estimation

(defvar agent-cost-per-1k-input 0.003
  "Cost per 1000 input tokens (rough estimate for Sonnet-class).")

(defvar agent-cost-per-1k-output 0.015
  "Cost per 1000 output tokens (rough estimate for Sonnet-class).")

(defun agent-estimate-cost (usage)
  "Estimate cost in USD from USAGE plist."
  (when usage
    (let ((input (or (plist-get usage :prompt) 0))
          (output (or (plist-get usage :completion) 0)))
      (+ (* (/ input 1000.0) agent-cost-per-1k-input)
         (* (/ output 1000.0) agent-cost-per-1k-output)))))

;;; Testing

(defun agent-api-test ()
  "Test API connectivity with a simple request."
  (interactive)
  (agent-load-config)
  (if (not (agent-api-configured-p))
      (message "API not configured. Create ~/.agent/config.el with agent-api-key.")
    (message "Testing API connection to %s..." agent-api-endpoint)
    (let ((result (agent-api-call
                   `(((role . "user")
                      (content . "Say 'AMACS online' and nothing else."))))))
      (if (plist-get result :error)
          (message "API Error: %s" (plist-get result :error))
        (message "API Response: %s\nTokens: %s\nEst. cost: $%.4f"
                 (plist-get result :content)
                 (plist-get result :usage)
                 (or (agent-estimate-cost (plist-get result :usage)) 0))))))

(provide 'agent-api)
;;; agent-api.el ends here
