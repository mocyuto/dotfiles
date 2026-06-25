---
root: true
targets:
  - '*'
globs:
  - '**/*'
---

# Mandatory Code Review

After completing any non-trivial code change (edit, write, bug fix, refactor,
new feature), you MUST invoke the `code-reviewer` agent as a quality gate
before declaring the task done.

Skip only when:
- The change is a trivial typo or formatting-only fix
- The user explicitly asks to skip review

Do not wait to be asked. Run the reviewer automatically.

# Tool Usage

- Searching: Use the grep tool.
- Reading file contents: Use the read tool.
