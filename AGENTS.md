# Commit messages

Write commit messages in this form:

```text
{scope}: {Imperative verb} ...

{Why this change is needed.}

Co-Authored-By: {Agent} ({Model})
```

`{scope}` is the package or the config area, as in `magit`, `avy`, `early-init`, `init`, or `custom`.

The subject is one line. Use the imperative mood, capitalize the verb, and leave off the final period.

The body states the reason in complete sentences, wrapped near 72 columns. Leave the body out when the subject already says the whole change.

Write the trailer as `Co-Authored-By: {Agent} ({Model})`. `{Agent}` is the display name, such as `Claude`, `Codex`, `Antigravity`, or `Grok Build`. `{Model}` is the full model name, such as `Claude Sonnet 5.5`, `GPT-6`, `Gemini 3.8 Flash`, or `Grok 4.7`. Put the model in parentheses and leave out an email address. Leave out reasoning-level qualifiers such as `High` or `Low`.
