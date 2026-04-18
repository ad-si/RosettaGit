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

1. Auto-repairs leftover wiki-to-markdown conversion artifacts in the source
   (`=={{header|X}}==` headings, `<lang>…</lang>` tags, `<pre style=…>` output
   blocks, prose-level `{{libheader|…}}` / `{{trans|…}}` / `{{works with|…}}` /
   `{{out}}` / etc.).
2. Runs preflight checks — see below. On failure, aborts *before* moving any
   files, so the source is left intact.
3. Moves `content/tasks/<task>.md` to `content/tasks/<task>/index.md` (if not
   already a page bundle).
4. Walks `## Language` sections, extracts every non-`txt` fenced code block
   into `content/tasks/<task>/<slug>.<ext>`, and replaces the block with a
   `{{ code(src=…, lang=…) }}` call.
5. Multiple blocks within one section get numeric suffixes
   (`javascript_1.js`, `javascript_2.js`).

## Preflight checks (built into the script)

Preflight runs automatically after the repair pass and before any file moves.
It aborts (exit code 2) if it finds any of:

- Unrepaired `<lang>` / `<pre>` tags or `==…==` wiki headings.
- Non-`{{ code(...) }}` wiki templates in prose (fenced content is exempt so
  Go template syntax etc. in output samples doesn't false-positive).
- Fenced code blocks with no language tag in implementation sections.
- Fenced code or `<lang>` buried inside HTML comments.

Non-blocking warnings are also reported:

- Frontmatter `languages = [...]` entries with no matching section, and
  sections whose slug isn't in `languages` (you're expected to reconcile
  these after the run).

Override with `--force` only after reviewing each reported issue. The typical
fix is to edit the source markdown and rerun without `--force`.

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
