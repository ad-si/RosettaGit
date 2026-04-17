---
name: extract-task-code
description: Extract each language implementation from a RosettaGit task markdown file into separate code files with proper extensions, and rewrite the task to include them via the `code` shortcode. Use when the user asks to split a task's embedded code blocks into co-located files, or "do the same thing for task X" following the twelve-days-of-christmas pattern.
---

# extract-task-code

Refactor a RosettaGit task so each language implementation lives in its own
file (with a proper extension) and the task's markdown loads each file via a
Zola shortcode instead of embedding code inline.

The end state for a task `content/tasks/<task>.md` is:

- `content/tasks/<task>/index.md` — the markdown, with every implementation
  code block replaced by `{{ code(src="...", lang="...") }}`
- `content/tasks/<task>/<language>.<ext>` — one file per implementation,
  containing only the code

Output blocks fenced as `txt` stay embedded in the markdown (they are sample
output, not implementations).

## Prerequisites

- `templates/shortcodes/code.md` must exist. If missing, create it:

  ```
  {%- set content = load_data(path=src, format="plain") -%}
  ```{{ lang }}
  {{ content | trim }}
  ```
  ```

## Usage

```bash
uv run --no-project python .claude/skills/extract-task-code/extract.py <task-name>
```

`<task-name>` is the markdown filename without `.md`
(e.g. `the_twelve_days_of_christmas`, `fizzbuzz`). Pass `--dry-run` to preview
without writing.

The script:

1. Moves `content/tasks/<task>.md` to `content/tasks/<task>/index.md` (if not
   already a page bundle).
2. Auto-repairs leftover wiki-to-markdown conversion artifacts in the source
   (`=={{header|X}}==` headings, `<lang>…</lang>` tags, `<pre style=…>` output
   blocks).
3. Walks `## Language` sections, extracts every non-`txt` fenced code block
   into `content/tasks/<task>/<slug>.<ext>`, and replaces the block with a
   `{{ code(src=…, lang=…) }}` call.
4. Multiple blocks within one section get numeric suffixes
   (`javascript_1.js`, `javascript_2.js`).

## Before running

- Confirm the task isn't already a page bundle.
- Read the task's frontmatter `languages = [...]` list to confirm the slugs.
- Skim the file for oddities the auto-repair may miss (e.g. wiki templates
  other than `{{header}}`, fenced blocks with no language, code inside HTML
  comments). Patch manually before running if needed.

## After running

- Check that the taxonomy `languages` list in the frontmatter covers every
  new section (add any that were hidden behind wiki-style headers).
- Spot-check two or three generated files: the extension should match the
  language and the file should contain only the implementation.
- Run `zola build` and verify the page renders. Pre-existing build failures
  in unrelated pages are not this skill's concern.

## Extension mapping

`extract.py` contains a `LANG_EXT` table mapping section-header titles to
`(slug, extension, fence-lang-hint)`. When a section title isn't in the
table, the script falls back to:

- slug: `slugify(title)` (lowercase, non-alnum → `_`)
- extension: `FENCE_EXT[fence_tag]` if known, else the fence tag itself

Fallback warnings are printed. After running, review them and, if the
inference was wrong, either edit the generated filename/fence manually or
add a proper `LANG_EXT` entry and re-run on a fresh copy.

## Testing

- `--dry-run` runs the extraction without writing files — use it to preview
  warnings and count before committing.
- Running the skill twice in a row is a no-op on an already-processed task
  (no code fences remain to extract).
