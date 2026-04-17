# Build Commands

- `zola serve` - Start development server
- `zola build` - Build static site to "public" directory
- `make serve` - Start development server (preferred)
- `make deploy` - Deploy website with Netlify


# Repository Structure

- `content/` - Markdown files for tasks and languages
  - `content/drafts/` - Tasks with issues to be fixed
  - `content/tasks/` - Active, working tasks
- `templates/` - HTML templates for site structure
- `static/` - Static assets like images
- `sass/` - SCSS stylesheets


# Style Guidelines

- Markdown should follow structure of existing tasks
- File naming: lowercase words with underscores (snake_case)
- When activating a draft, fix formatting issues before moving from drafts to tasks
- Maintain same formatting and structure as other task pages
- Reference existing tasks for proper formatting structure


# Wiki to Markdown Conversion

- Convert `[[...]]` links to Markdown links `[text](link)`
- Replace `== Heading ==` with `## Heading` (for all heading levels)
- Replace `{{...}}` template tags with appropriate Markdown equivalents
- Convert wiki-style numbered lists (`::#`) to Markdown ordered lists (`1.`)
- Replace `'''Bold'''` with `**Bold**` and `''Italic''` with `*Italic*`
- Convert wiki tables to Markdown tables using pipe syntax
