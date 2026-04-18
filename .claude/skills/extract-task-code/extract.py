#!/usr/bin/env python3
"""Extract language implementations from a RosettaGit task into co-located files.

Usage:
    python extract.py <task-name> [--dry-run]

Where <task-name> is the basename of content/tasks/<task-name>.md (or an
existing page bundle directory content/tasks/<task-name>/).

The script converts the task into a page bundle, moves each language
implementation's code block into a sibling file with the correct extension,
and replaces the inline fence with a `{{ code(src=..., lang=...) }}` call.
Blocks fenced as `txt` (sample output) stay embedded in the markdown.
"""

from __future__ import annotations

import argparse
import re
import sys
import textwrap
from pathlib import Path


# Section-header title -> (slug, extension, default fence lang hint).
# The slug becomes the filename stem and should match the taxonomy entry.
# Extension: canonical extension for the language.
# Fence lang: preferred value for the shortcode's `lang` argument when the
# original fence tag is missing or unhelpful.
LANG_EXT: dict[str, tuple[str, str, str]] = {
    "6502 Assembly": ("6502_assembly", "asm", "asm"),
    "68000 Assembly": ("68000_assembly", "asm", "asm"),
    "8086 Assembly": ("8086_assembly", "asm", "asm"),
    "ABAP": ("abap", "abap", "abap"),
    "ActionScript": ("actionscript", "as", "actionscript"),
    "Ada": ("ada", "adb", "ada"),
    "ALGOL 68": ("algol_68", "a68", "algol68"),
    "ALGOL W": ("algol_w", "alw", "algol"),
    "APL": ("apl", "apl", "apl"),
    "AppleScript": ("applescript", "applescript", "applescript"),
    "Arturo": ("arturo", "art", "arturo"),
    "AutoHotkey": ("autohotkey", "ahk", "autohotkey"),
    "AutoIt": ("autoit", "au3", "autoit"),
    "AWK": ("awk", "awk", "awk"),
    "Babel": ("babel", "babel", "babel"),
    "BASIC": ("basic", "bas", "basic"),
    "BASIC256": ("basic256", "bas", "basic"),
    "Batch File": ("batch_file", "bat", "dos"),
    "BBC BASIC": ("bbc_basic", "bas", "basic"),
    "Befunge": ("befunge", "befunge", "befunge"),
    "Bracmat": ("bracmat", "bra", "bracmat"),
    "C": ("c", "c", "c"),
    "Caché ObjectScript": ("cache_objectscript", "cos", "cos"),
    "C#": ("csharp", "cs", "csharp"),
    "C++": ("cpp", "cpp", "cpp"),
    "Ceylon": ("ceylon", "ceylon", "ceylon"),
    "Clojure": ("clojure", "clj", "clojure"),
    "CoffeeScript": ("coffeescript", "coffee", "coffeescript"),
    "COBOL": ("cobol", "cob", "cobol"),
    "Common Lisp": ("common_lisp", "lisp", "lisp"),
    "Crystal": ("crystal", "cr", "crystal"),
    "D": ("d", "d", "d"),
    "Dart": ("dart", "dart", "dart"),
    "dc": ("dc", "dc", "dc"),
    "Delphi": ("delphi", "dpr", "delphi"),
    "DWScript": ("dwscript", "pas", "pascal"),
    "Dyalect": ("dyalect", "dy", "dyalect"),
    "E": ("e", "e", "e"),
    "EchoLisp": ("echolisp", "lisp", "lisp"),
    "Eiffel": ("eiffel", "e", "eiffel"),
    "Elena": ("elena", "l", "elena"),
    "Elixir": ("elixir", "exs", "elixir"),
    "Elm": ("elm", "elm", "elm"),
    "Emacs Lisp": ("emacs_lisp", "el", "lisp"),
    "Erlang": ("erlang", "erl", "erlang"),
    "ERRE": ("erre", "erre", "erre"),
    "Euphoria": ("euphoria", "e", "euphoria"),
    "F#": ("fsharp", "fs", "fsharp"),
    "Factor": ("factor", "factor", "factor"),
    "Fantom": ("fantom", "fan", "fantom"),
    "Forth": ("forth", "4th", "forth"),
    "Fortran": ("fortran", "f90", "fortran"),
    "FreeBASIC": ("freebasic", "bas", "freebasic"),
    "Frink": ("frink", "frink", "frink"),
    "FutureBasic": ("futurebasic", "bas", "basic"),
    "Gambas": ("gambas", "gambas", "gambas"),
    "GAP": ("gap", "g", "gap"),
    "GFA Basic": ("gfa_basic", "gfa", "basic"),
    "Go": ("go", "go", "go"),
    "Groovy": ("groovy", "groovy", "groovy"),
    "Harbour": ("harbour", "prg", "harbour"),
    "Haskell": ("haskell", "hs", "haskell"),
    "HicEst": ("hicest", "hic", "hicest"),
    "Icon": ("icon", "icn", "icon"),
    "Icon and Unicon": ("unicon", "icn", "unicon"),
    "Idris": ("idris", "idr", "idris"),
    "J": ("j", "ijs", "j"),
    "Java": ("java", "java", "java"),
    "JavaScript": ("javascript", "js", "javascript"),
    "jq": ("jq", "jq", "jq"),
    "Jsish": ("jsish", "jsi", "javascript"),
    "Julia": ("julia", "jl", "julia"),
    "Kotlin": ("kotlin", "kt", "kotlin"),
    "LabVIEW": ("labview", "vi", "labview"),
    "Lasso": ("lasso", "lasso", "lasso"),
    "LFE": ("lfe", "lfe", "lfe"),
    "Liberty BASIC": ("liberty_basic", "bas", "basic"),
    "Lingo": ("lingo", "lingo", "lingo"),
    "Lisp": ("lisp", "lisp", "lisp"),
    "LiveCode": ("livecode", "livecode", "livecode"),
    "Logo": ("logo", "lg", "logo"),
    "Logtalk": ("logtalk", "lgt", "logtalk"),
    "LOLCODE": ("lolcode", "lol", "lolcode"),
    "Lua": ("lua", "lua", "lua"),
    "M2000 Interpreter": ("m2000_interpreter", "m2000", "basic"),
    "M4": ("m4", "m4", "m4"),
    "Maple": ("maple", "mpl", "maple"),
    "Mathematica": ("mathematica", "m", "mathematica"),
    "MATLAB": ("matlab", "m", "matlab"),
    "Maxima": ("maxima", "mac", "maxima"),
    "Mercury": ("mercury", "m", "mercury"),
    "MiniScript": ("miniscript", "ms", "miniscript"),
    "MIPS Assembly": ("mips_assembly", "asm", "asm"),
    "Modula-2": ("modula_2", "mod", "modula2"),
    "Modula-3": ("modula_3", "m3", "modula3"),
    "MontiLang": ("montilang", "mlg", "montilang"),
    "N/t/roff": ("nroff", "nroff", "nroff"),
    "Neko": ("neko", "neko", "neko"),
    "Nemerle": ("nemerle", "n", "nemerle"),
    "NetRexx": ("netrexx", "nrx", "netrexx"),
    "NewLISP": ("newlisp", "lsp", "lisp"),
    "Nim": ("nim", "nim", "nim"),
    "Objeck": ("objeck", "obs", "objeck"),
    "Objective-C": ("objective_c", "m", "objc"),
    "OCaml": ("ocaml", "ml", "ocaml"),
    "Octave": ("octave", "m", "octave"),
    "Oforth": ("oforth", "of", "oforth"),
    "ooRexx": ("oorexx", "rex", "rexx"),
    "Oz": ("oz", "oz", "oz"),
    "PARI/GP": ("pari_gp", "gp", "parigp"),
    "Pascal": ("pascal", "pas", "pascal"),
    "Perl": ("perl", "pl", "perl"),
    "Perl 6": ("perl_6", "p6", "perl6"),
    "Phix": ("phix", "exw", "phix"),
    "PHP": ("php", "php", "php"),
    "PicoLisp": ("picolisp", "l", "picolisp"),
    "Pike": ("pike", "pike", "pike"),
    "PL/I": ("pl_i", "pli", "pli"),
    "Pop11": ("pop11", "p", "pop11"),
    "PostScript": ("postscript", "ps", "postscript"),
    "PowerShell": ("powershell", "ps1", "powershell"),
    "Processing": ("processing", "pde", "processing"),
    "Prolog": ("prolog", "pro", "prolog"),
    "PureBasic": ("purebasic", "pb", "purebasic"),
    "Python": ("python", "py", "python"),
    "QB64": ("qb64", "bas", "basic"),
    "R": ("r", "r", "r"),
    "Racket": ("racket", "rkt", "racket"),
    "Raku": ("raku", "raku", "raku"),
    "Rascal": ("rascal", "rsc", "rascal"),
    "Rebol": ("rebol", "reb", "rebol"),
    "Red": ("red", "red", "red"),
    "Retro": ("retro", "retro", "retro"),
    "REXX": ("rexx", "rexx", "rexx"),
    "Ring": ("ring", "ring", "ring"),
    "RLaB": ("rlab", "r", "rlab"),
    "Ruby": ("ruby", "rb", "ruby"),
    "Run BASIC": ("run_basic", "bas", "runbasic"),
    "Rust": ("rust", "rs", "rust"),
    "Scala": ("scala", "scala", "scala"),
    "Scheme": ("scheme", "scm", "scheme"),
    "Seed7": ("seed7", "sd7", "seed7"),
    "Self": ("self", "self", "self"),
    "SequenceL": ("sequencel", "sl", "sequencel"),
    "Sidef": ("sidef", "sf", "sidef"),
    "Simula": ("simula", "sim", "simula"),
    "Sinclair ZX81 BASIC": ("sinclair_zx81_basic", "bas", "basic"),
    "Smalltalk": ("smalltalk", "st", "smalltalk"),
    "Smart BASIC": ("smart_basic", "bas", "smart-basic"),
    "SNOBOL4": ("snobol4", "sno", "snobol"),
    "Snobol": ("snobol", "sno", "snobol"),
    "SparForte": ("sparforte", "sf", "sparforte"),
    "SQL": ("sql", "sql", "sql"),
    "Standard ML": ("standard_ml", "ml", "sml"),
    "Stata": ("stata", "do", "stata"),
    "Swift": ("swift", "swift", "swift"),
    "Tailspin": ("tailspin", "tt", "tailspin"),
    "Tcl": ("tcl", "tcl", "tcl"),
    "TI-83 BASIC": ("ti_83_basic", "8xp", "basic"),
    "TI-89 BASIC": ("ti_89_basic", "89p", "basic"),
    "TXR": ("txr", "txr", "txr"),
    "TypeScript": ("typescript", "ts", "typescript"),
    "UNIX Shell": ("unix_shell", "sh", "bash"),
    "Ursa": ("ursa", "ursa", "ursa"),
    "Ursala": ("ursala", "fun", "ursala"),
    "V": ("v", "v", "v"),
    "Vala": ("vala", "vala", "vala"),
    "VBA": ("vba", "vba", "vb"),
    "VBScript": ("vbscript", "vbs", "vb"),
    "Vedit macro language": ("vedit_macro_language", "vdm", "vedit"),
    "Visual Basic": ("visual_basic", "vb", "vb"),
    "Visual Basic .NET": ("visual_basic_dotnet", "vb", "vbnet"),
    "Wart": ("wart", "wart", "wart"),
    "Wortel": ("wortel", "wl", "wortel"),
    "Wren": ("wren", "wren", "wren"),
    "XPL0": ("xpl0", "xpl", "xpl0"),
    "Yabasic": ("yabasic", "yab", "basic"),
    "Z80 Assembly": ("z80_assembly", "asm", "asm"),
    "Zig": ("zig", "zig", "zig"),
    "zkl": ("zkl", "zkl", "zkl"),
    "Zsh": ("zsh", "zsh", "bash"),
    "ZX Spectrum Basic": ("zx_spectrum_basic", "bas", "basic"),
}

SKIP_SECTIONS = {"Task", "Related tasks", "See also", "References", "Example"}


# Fallback: fence-tag (lowercased) -> extension. Used when a section header is
# not in LANG_EXT but the fence identifies the language.
FENCE_EXT: dict[str, str] = {
    "bash": "sh",
    "basic": "bas",
    "brainfuck": "bf",
    "clojure": "clj",
    "csharp": "cs",
    "c": "c",
    "cpp": "cpp",
    "elisp": "el",
    "elm": "elm",
    "erlang": "erl",
    "fsharp": "fs",
    "go": "go",
    "haskell": "hs",
    "java": "java",
    "javascript": "js",
    "julia": "jl",
    "kotlin": "kt",
    "lisp": "lisp",
    "lua": "lua",
    "nim": "nim",
    "ocaml": "ml",
    "pascal": "pas",
    "perl": "pl",
    "php": "php",
    "powershell": "ps1",
    "prolog": "pro",
    "python": "py",
    "racket": "rkt",
    "ruby": "rb",
    "rust": "rs",
    "scala": "scala",
    "scheme": "scm",
    "sh": "sh",
    "sql": "sql",
    "swift": "swift",
    "tcl": "tcl",
    "typescript": "ts",
    "vb": "vb",
    "vbnet": "vb",
    "zsh": "zsh",
}


def slugify(title: str) -> str:
    """Lowercase, replace non-alnum runs with `_`, strip edges."""
    s = re.sub(r"[^a-zA-Z0-9]+", "_", title.lower()).strip("_")
    return s or "section"


def normalize_code(code: str) -> str:
    """Drop leading/trailing blank lines and strip common indentation."""
    lines = code.split("\n")
    while lines and not lines[0].strip():
        lines.pop(0)
    while lines and not lines[-1].strip():
        lines.pop()
    if not lines:
        return ""
    return textwrap.dedent("\n".join(lines))


def repair_wiki_leftovers(text: str) -> str:
    """Fix common wiki-to-markdown conversion artifacts before parsing."""
    # =={{header|Foo|F#}}== -> ## F# (uses the DISPLAY name, last pipe segment)
    def header_repl(m: re.Match[str]) -> str:
        parts = m.group(1).split("|")
        name = parts[-1] if len(parts) > 1 else parts[0]
        return f"## {name}"

    text = re.sub(r"^==\s*\{\{\s*header\|([^}]+)\}\}\s*==\s*$",
                  header_repl, text, flags=re.M)

    # =={{header|Icon}} and {{header|Unicon}}== -> ## Icon and Unicon
    def combined_repl(m: re.Match[str]) -> str:
        names = re.findall(r"\{\{\s*header\|([^}|]+)(?:\|[^}]+)?\}\}", m.group(1))
        joined = " and ".join(names) if names else m.group(1)
        return f"## {joined}"

    text = re.sub(r"^==\s*(.*\{\{\s*header\|[^}]+\}\}.*)==\s*$",
                  combined_repl, text, flags=re.M)

    def _fence_from_lang(raw: str) -> str:
        # Whitespace inside a <lang ...> arg (e.g. `<lang X86_64 Assembly>`)
        # is collapsed to `_` so the resulting fence tag is a single token.
        return re.sub(r"\s+", "_", raw.strip())

    # <lang LANG>…</lang> -> ```LANG\n…\n```
    text = re.sub(
        r"<lang\s+([^>]+)>(.*?)</lang>",
        lambda m: f"```{_fence_from_lang(m.group(1))}\n{m.group(2).strip()}\n```",
        text,
        flags=re.S,
    )
    # <lang>…</lang> -> ```\n…\n```
    text = re.sub(
        r"<lang\s*>(.*?)</lang>",
        lambda m: f"```\n{m.group(1).strip()}\n```",
        text,
        flags=re.S,
    )
    # Half-broken <lang …>CODE with later standalone ``` close (seen in the
    # twelve-days task). Only the opener is present; replace it with a plain
    # fence and let the pre-existing ``` close match up.
    text = re.sub(
        r"^<lang\s+([^>]+)>",
        lambda m: f"```{_fence_from_lang(m.group(1))}",
        text,
        flags=re.M,
    )
    text = re.sub(r"^<lang\s*>", r"```", text, flags=re.M)

    # <pre style="…">CONTENT -> ```txt\nCONTENT
    text = re.sub(
        r"<pre[^>]*>(\s*\n)?",
        lambda m: "```txt\n",
        text,
    )
    # </pre> -> ```  (in case any closes survived)
    text = re.sub(r"</pre>", "```", text)

    # Strip common prose-level wiki templates that carry metadata but no
    # structural meaning in markdown. Keep them on their own line if possible.
    # {{works with|Foo}} / {{libheader|Bar}} / {{trans|C}} / {{out}} / {{in}}
    # / {{omit from|X}}
    text = re.sub(
        r"^\s*\{\{\s*(?:works with|libheader|trans|translation of|out|output|in|"
        r"uses from|requires|header|omit from)\s*(?:\|[^}]*)?\}\}\s*$",
        "",
        text,
        flags=re.M | re.I,
    )
    return text


def parse_sections(lines: list[str]) -> list[tuple[str, int, int]]:
    """Return [(title, start_line, end_line)] for each level-2 section."""
    sections = []
    current_title: str | None = None
    current_start: int | None = None
    for i, line in enumerate(lines):
        m = re.match(r"^## (?!#)(.+?)\s*$", line)
        if m:
            if current_title is not None:
                assert current_start is not None
                sections.append((current_title, current_start, i))
            current_title = m.group(1).strip()
            current_start = i
    if current_title is not None:
        assert current_start is not None
        sections.append((current_title, current_start, len(lines)))
    return sections


def extract_code_blocks(
    section_lines: list[str],
) -> list[tuple[int, int, str, str]]:
    """Return [(start, end, fence_lang, content)] for each fenced block."""
    blocks = []
    in_fence = False
    fence_lang: str | None = None
    fence_start: int | None = None
    code_lines: list[str] = []
    for i, line in enumerate(section_lines):
        if not in_fence:
            m = re.match(r"^```(.*)$", line)
            if m:
                in_fence = True
                fence_lang = m.group(1).strip()
                fence_start = i
                code_lines = []
        else:
            if line.rstrip() == "```":
                assert fence_start is not None
                blocks.append((fence_start, i + 1, fence_lang or "", "\n".join(code_lines)))
                in_fence = False
                fence_lang = None
                fence_start = None
                code_lines = []
            else:
                code_lines.append(line)
    return blocks


def locate_source(root: Path, task_name: str) -> tuple[Path, Path, Path, bool]:
    """Return (tasks_dir, source_file, bundle_dir, already_bundle).

    Does NOT move or create anything — that happens later after preflight passes.
    """
    tasks_dir = root / "content" / "tasks"
    single_md = tasks_dir / f"{task_name}.md"
    bundle_dir = tasks_dir / task_name
    index_md = bundle_dir / "index.md"

    if bundle_dir.is_dir() and index_md.is_file():
        return tasks_dir, index_md, bundle_dir, True
    if single_md.is_file():
        return tasks_dir, single_md, bundle_dir, False
    raise SystemExit(
        f"Task not found: neither {single_md} nor {index_md} exists."
    )


def parse_frontmatter_languages(text: str) -> list[str] | None:
    """Return the `languages = [...]` list from TOML frontmatter, or None."""
    fm_match = re.match(r"\+\+\+\n(.*?)\n\+\+\+", text, re.S)
    if not fm_match:
        return None
    body = fm_match.group(1)
    langs_match = re.search(r"^languages\s*=\s*\[(.*?)\]", body, re.S | re.M)
    if not langs_match:
        return None
    return re.findall(r'"([^"]+)"', langs_match.group(1))


def preflight_checks(
    text: str,
    sections: list[tuple[str, int, int]],
    lines: list[str],
) -> tuple[list[str], list[str]]:
    """Scan for issues that would produce wrong output. Returns (blockers, warnings)."""
    blockers: list[str] = []
    warnings: list[str] = []

    # Strip HTML comments before other checks so their contents don't trigger
    # false positives — but flag any fenced code buried inside them.
    for m in re.finditer(r"<!--(.*?)-->", text, re.S):
        if "```" in m.group(1) or "<lang" in m.group(1):
            line_no = text[: m.start()].count("\n") + 1
            blockers.append(
                f"line {line_no}: fenced code or <lang> tag inside an HTML comment; "
                "remove the comment wrapping before extracting."
            )
    stripped = re.sub(r"<!--.*?-->", "", text, flags=re.S)

    # Unrepaired wiki artifacts (repair pass should have handled these).
    for tag, label in (
        (r"<lang\b", "<lang> opener"),
        (r"</lang>", "</lang> closer"),
        (r"<pre\b", "<pre> opener"),
        (r"</pre>", "</pre> closer"),
    ):
        for m in re.finditer(tag, stripped):
            line_no = stripped[: m.start()].count("\n") + 1
            blockers.append(f"line {line_no}: unrepaired {label}")

    # Surviving wiki-style headers (==…==).
    for i, line in enumerate(lines, 1):
        if re.match(r"^==[^=].*==\s*$", line):
            blockers.append(f"line {i}: unrepaired wiki heading {line.strip()!r}")

    # Non-header `{{…}}` wiki templates. Scan only prose (outside fenced code
    # blocks) — text inside fences may legitimately contain `{{...}}` (e.g. Go
    # templates, Tera samples). Allow the `{{ code(...) }}` shortcode.
    prose_only = re.sub(r"(?m)^```.*?^```\s*$", "", stripped, flags=re.S)
    for m in re.finditer(r"\{\{([^}]+)\}\}", prose_only):
        inner = m.group(1).strip()
        if inner.startswith("code(") or inner.startswith("code "):
            continue
        line_no = prose_only[: m.start()].count("\n") + 1
        blockers.append(
            f"line {line_no}: wiki template still present: {{{{{inner}}}}}"
        )

    # Fenced blocks with no language tag in any implementation section.
    for title, start, end in sections:
        if title in SKIP_SECTIONS:
            continue
        section_slice = lines[start:end]
        for block_start, _, flang, _ in extract_code_blocks(section_slice):
            if not flang.strip():
                blockers.append(
                    f"line {start + block_start + 1}: fenced code block with no "
                    f"language tag in section {title!r}"
                )
    return blockers, warnings


def collect_shortcode_slugs(text: str, task: str) -> list[str]:
    """Return the sorted language slugs referenced by `{{ code(src=...) }}`
    shortcodes for the given task. Collapses multi-block suffixes: if the text
    contains both `foo_1.ext` and `foo_2.ext`, the slug is `foo`; a lone
    `xslt_2_0.ext` (no `xslt_2_1` sibling) keeps its trailing digits.
    """
    filenames = re.findall(
        rf'code\(src="content/tasks/{re.escape(task)}/([^"]+)"',
        text,
    )
    stems = {re.sub(r"\.[^.]+$", "", f) for f in filenames}
    slugs: set[str] = set()
    for stem in stems:
        m = re.match(r"(.+)_(\d+)$", stem)
        if m:
            base = m.group(1)
            has_siblings = any(
                other != stem and re.fullmatch(rf"{re.escape(base)}_\d+", other)
                for other in stems
            )
            if has_siblings:
                slugs.add(base)
                continue
        slugs.add(stem)
    return sorted(slugs)


def sync_frontmatter_languages(text: str, slugs: list[str]) -> str:
    """Replace the `languages = [...]` list in TOML frontmatter with `slugs`.
    No-op if frontmatter or the languages key is missing."""
    lang_block = (
        "languages = [\n"
        + "".join(f'  "{s}",\n' for s in slugs)
        + "]"
    )
    new_text, n = re.subn(
        r"^languages\s*=\s*\[.*?\n\]",
        lang_block,
        text,
        count=1,
        flags=re.S | re.M,
    )
    return new_text if n else text


def cross_check_frontmatter(
    frontmatter_langs: list[str] | None,
    section_slugs: list[str],
) -> list[str]:
    """Warn about frontmatter/section mismatches. Non-blocking — the skill's
    after-run step is to reconcile these."""
    if frontmatter_langs is None:
        return []
    fm = set(frontmatter_langs)
    sec = set(section_slugs)
    warnings = []
    for missing in sorted(sec - fm):
        warnings.append(
            f"section extracts to {missing!r} but it's not in frontmatter languages"
        )
    for stale in sorted(fm - sec):
        warnings.append(
            f"frontmatter lists {stale!r} but no matching section was found"
        )
    return warnings


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("task", help="task filename without .md")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--force",
        action="store_true",
        help="Proceed even if preflight blockers are detected. Use only after "
             "reviewing the reported issues.",
    )
    parser.add_argument(
        "--root",
        default=None,
        help="Site root (defaults to git root; falls back to cwd).",
    )
    parser.add_argument(
        "--sync-frontmatter",
        action="store_true",
        help="After extracting, rewrite the frontmatter `languages = [...]` "
             "list to match the emitted files. Also works on an already-"
             "bundled task (no code blocks to extract).",
    )
    args = parser.parse_args()

    if args.root:
        root = Path(args.root).resolve()
    else:
        # Walk upwards looking for config.toml so the skill works from any
        # cwd, then fall back to this file's project root.
        cwd = Path.cwd()
        root = None
        for parent in (cwd, *cwd.parents):
            if (parent / "config.toml").is_file():
                root = parent
                break
        if root is None:
            raise SystemExit("Could not find config.toml up the tree; pass --root.")

    _, source_file, bundle_dir, already = locate_source(root, args.task)
    text = source_file.read_text()
    repaired = repair_wiki_leftovers(text)
    if repaired != text:
        print("Repaired wiki-to-markdown leftovers.")
        text = repaired

    lines = text.split("\n")
    sections = parse_sections(lines)

    # Preflight: scan for problems the extractor can't safely handle. Runs
    # BEFORE any file moves so a failing check leaves the source untouched.
    blockers, pre_warnings = preflight_checks(text, sections, lines)
    frontmatter_langs = parse_frontmatter_languages(text)
    section_slugs = [
        LANG_EXT[t][0] if t in LANG_EXT else slugify(t)
        for t, _, _ in sections
        if t not in SKIP_SECTIONS
    ]
    fm_warnings = cross_check_frontmatter(frontmatter_langs, section_slugs)

    for w in pre_warnings + fm_warnings:
        print(f"PREFLIGHT WARN: {w}", file=sys.stderr)
    if blockers:
        for b in blockers:
            print(f"PREFLIGHT BLOCK: {b}", file=sys.stderr)
        if not args.force:
            print(
                f"\nAborting: {len(blockers)} preflight blocker(s). Fix them "
                "in the source file, or re-run with --force to override.",
                file=sys.stderr,
            )
            return 2
        print("Continuing despite blockers due to --force.", file=sys.stderr)

    # Convert to page bundle (if needed) only after preflight passes.
    index_md = bundle_dir / "index.md"
    if not already and not args.dry_run:
        bundle_dir.mkdir(exist_ok=True)
        source_file.rename(index_md)

    # Process sections in reverse so splices don't shift later indices.
    new_lines = list(lines)
    total_files = 0
    warnings: list[str] = []
    per_slug_counters: dict[str, int] = {}

    for title, start, end in reversed(sections):
        if title in SKIP_SECTIONS:
            continue
        section_slice = lines[start:end]
        blocks = extract_code_blocks(section_slice)
        impl_blocks = [b for b in blocks if b[2].lower() != "txt"]
        if not impl_blocks:
            continue

        if title in LANG_EXT:
            slug, ext, default_lang = LANG_EXT[title]
        else:
            slug = slugify(title)
            fence_tag = impl_blocks[0][2].strip().lower()
            if fence_tag in FENCE_EXT:
                ext = FENCE_EXT[fence_tag]
            elif fence_tag:
                ext = slugify(fence_tag)
            else:
                ext = "txt"
            default_lang = fence_tag or slug
            warnings.append(
                f"Unknown section {title!r}: inferred slug={slug!r} ext=.{ext}"
            )

        impl_idx = {b[0]: b for b in impl_blocks}
        impl_count = len(impl_blocks)
        rewritten: list[str] = []
        idx = 0
        file_counter = 0
        while idx < len(section_slice):
            if idx in impl_idx:
                _, be, flang, code = impl_idx[idx]
                file_counter += 1
                if impl_count == 1:
                    filename = f"{slug}.{ext}"
                else:
                    filename = f"{slug}_{file_counter}.{ext}"
                target = bundle_dir / filename
                if not args.dry_run:
                    target.write_text(normalize_code(code) + "\n")
                total_files += 1
                shortcode_lang = flang if flang else default_lang
                src = f"content/tasks/{args.task}/{filename}"
                rewritten.append("")
                rewritten.append(
                    f'{{{{ code(src="{src}", lang="{shortcode_lang}") }}}}'
                )
                rewritten.append("")
                idx = be
            else:
                rewritten.append(section_slice[idx])
                idx += 1
        new_lines[start:end] = rewritten

    output = "\n".join(new_lines)
    if args.sync_frontmatter:
        slugs = collect_shortcode_slugs(output, args.task)
        if slugs:
            output = sync_frontmatter_languages(output, slugs)
            print(
                f"{'Would sync' if args.dry_run else 'Synced'} frontmatter "
                f"languages ({len(slugs)} entries)."
            )
        else:
            print(
                "No `code(...)` shortcodes found; skipping frontmatter sync.",
                file=sys.stderr,
            )

    if not args.dry_run:
        index_md.write_text(output)
    print(f"{'Would write' if args.dry_run else 'Wrote'} {total_files} code files.")
    if not already and not args.dry_run:
        print(f"Converted to page bundle: {bundle_dir}")
    for w in warnings:
        print(f"WARN: {w}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
