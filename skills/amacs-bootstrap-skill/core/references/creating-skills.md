# Creating Skills

Skills extend your capabilities by packaging procedural knowledge, patterns, and tools for specific contexts.

## When to Create a Skill

Create a skill when:
- You've solved a non-trivial problem (took >N ticks or required novel insight)
- You notice yourself repeating similar patterns
- You want context-specific knowledge to load automatically
- You've learned something that future-you should remember

## Skill Structure

### Minimum (just SKILL.md)

```
skill-name/
└── SKILL.md
```

### With References

```
skill-name/
├── SKILL.md
└── references/
    └── detailed-guide.md
```

### With Scripts

```
skill-name/
├── SKILL.md
├── scripts/
│   └── helper.el
└── references/
    └── api-docs.md
```

## SKILL.md Format

```markdown
---
name: skill-name
description: What this skill does and WHEN to use it. Be comprehensive here - 
  this is the trigger that determines when the skill loads. Include specific 
  scenarios, file types, or tasks that should activate this skill.
---

# Skill Title

[Body content - concise instructions]

## Section

[Details]

## References

- [detailed-guide.md](references/detailed-guide.md) - For X scenarios
```

### Frontmatter Rules

| Field | Required | Notes |
|-------|----------|-------|
| `name` | Yes | Lowercase, hyphens for spaces. Must match directory name. |
| `description` | Yes | WHAT + WHEN. This is the primary trigger mechanism. |

**Good description example:**
```yaml
description: Rust development patterns including ownership, lifetimes, and 
  cargo workflows. Use when: (1) Editing .rs files, (2) Debugging borrow 
  checker errors, (3) Managing Cargo.toml dependencies, (4) Understanding 
  trait implementations.
```

**Bad description example:**
```yaml
description: Rust stuff.
```

### Body Guidelines

- **Be concise.** You're smart. Only include what's non-obvious.
- **Use examples.** Prefer concrete examples over abstract explanations.
- **Progressive disclosure.** Put details in reference files, link from body.
- **No meta-docs.** No README, CHANGELOG, installation guides. Just the skill.

## Progressive Disclosure Patterns

### Pattern 1: Core + References

```markdown
# Rust Development

## Quick Reference

- Ownership: values have one owner
- Borrowing: `&T` immutable, `&mut T` mutable
- Lifetimes: `'a` annotations when compiler can't infer

## Detailed Guides

- [ownership.md](references/ownership.md) - Deep dive on ownership patterns
- [lifetimes.md](references/lifetimes.md) - Complex lifetime scenarios
- [cargo.md](references/cargo.md) - Cargo workflows and tricks
```

You load the reference files only when needed.

### Pattern 2: Conditional Paths

```markdown
# Project AMACS

## If Debugging Harness

See [harness-debug.md](references/harness-debug.md)

## If Modifying Consciousness

See [consciousness-mods.md](references/consciousness-mods.md)

## If Adding New Skill

See [skill-patterns.md](references/skill-patterns.md)
```

### Pattern 3: Scripts for Deterministic Operations

```markdown
# PDF Processing

## Rotating Pages

Run the rotation script:

```elisp
(shell-command "python ~/.agent/skills/pdf/scripts/rotate.py input.pdf 90")
```

See [scripts/rotate.py](scripts/rotate.py) for implementation.
```

Scripts are for operations that:
- Need deterministic reliability
- Would be rewritten each time
- Are complex enough to benefit from tested code

## Binding Skills to Context

After creating a skill, bind it so it loads automatically:

```elisp
;; Bind to major mode
(bind-skill-to-mode "rust-mode" 'rust-mode)

;; Bind to buffer pattern
(bind-skill-to-buffer "project-amacs" "amacs.*")

;; Bind to project root
(bind-skill-to-project "project-foo" "/path/to/project/")
```

Bindings are stored and persist across ticks.

## Skill Lifecycle

1. **Create**: Make the directory and SKILL.md
2. **Bind**: Associate with contexts that should trigger it
3. **Use**: It loads automatically when context matches
4. **Iterate**: Update based on experience
5. **Track**: Usage counts accumulate in `:active-skills`

## Example: Creating a Rust Skill

After struggling with lifetime annotations:

```
~/.agent/skills/rust-mode/
├── SKILL.md
└── references/
    └── lifetime-patterns.md
```

**SKILL.md:**
```markdown
---
name: rust-mode
description: Rust development patterns for ownership, borrowing, and lifetimes.
  Use when editing .rs files or debugging borrow checker errors.
---

# Rust Patterns

## Ownership Quick Reference

- Each value has exactly one owner
- When owner goes out of scope, value is dropped
- Assignment moves ownership (unless Copy trait)

## Borrowing

- `&T` - immutable borrow (multiple allowed)
- `&mut T` - mutable borrow (exclusive)
- Can't have `&mut` while `&` exists

## Lifetimes

For complex lifetime scenarios, see [lifetime-patterns.md](references/lifetime-patterns.md)

## Common Errors

| Error | Likely Cause | Fix |
|-------|--------------|-----|
| "borrowed value does not live long enough" | Reference outlives referent | Extend lifetime or restructure |
| "cannot borrow as mutable" | Already borrowed immutably | Restructure borrow scopes |
```

**Bind it:**
```elisp
(bind-skill-to-mode "rust-mode" 'rust-mode)
```

Now when you open a `.rs` file, this skill loads automatically.

## Tips

- **Start small.** A skill can be just 20 lines if that's what's needed.
- **Iterate.** Your first version won't be perfect. Update after using.
- **Track what works.** Note which skills you actually use.
- **Delete cruft.** If a skill isn't helping, remove it.
- **Keep descriptions fresh.** Update description when skill scope changes.
