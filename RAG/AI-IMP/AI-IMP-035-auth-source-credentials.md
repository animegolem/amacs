---
node_id: AI-IMP-035
tags:
  - IMP-LIST
  - Implementation
  - security
  - credentials
  - auth-source
kanban_status: complete
depends_on: []
confidence_score: 0.95
created_date: 2025-12-30
close_date: 2025-12-30
priority: low
---

# AI-IMP-035: Auth-Source Credential Management

## Summary

Migrate API key retrieval from hardcoded config to Emacs auth-source, per RFC Part 15.

**Current state:** API key via env var or `~/.agent/config.el` with plaintext key.

**Target state:** Auth-source integration supporting `~/.authinfo.gpg`, GNOME Keyring, macOS Keychain, etc.

**Why:**
- Credentials stored encrypted, not plaintext
- Works with existing Emacs security infrastructure
- Env var fallback preserved for CI/containers

**Done when:** `agent--get-api-key` retrieves from auth-source with env var fallback.

### Design (from RFC Part 15)

```elisp
(defun agent--get-api-key ()
  "Retrieve API key from auth-source."
  (let ((found (car (auth-source-search
                     :host "openrouter.ai"
                     :user "amacs"
                     :require '(:secret)))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret) (funcall secret) secret)))))

(defun agent-load-api-key ()
  "Load API key from best available source."
  (or (getenv "OPENROUTER_API_KEY")
      (agent--get-api-key)
      (user-error "No API key found. See docs for setup.")))
```

### Implementation Checklist

- [ ] Add `(require 'auth-source)` to agent-api.el
- [ ] Implement `agent--get-api-key` using auth-source-search
- [ ] Update `agent-load-config` to use fallback chain
- [ ] Document setup in README (authinfo.gpg, GNOME Keyring examples)
- [ ] Test with ~/.authinfo.gpg
- [ ] Test env var still works as fallback

### Priority

Low - current env var approach works fine. This is UX polish for users who prefer encrypted credential storage.
