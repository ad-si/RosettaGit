#!/usr/bin/env python3
"""Create minimal stub pages for language slugs referenced by tasks but
without a dedicated page under content/languages/.

Title mapping is hand-curated from the slug → canonical display name.
"""
from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LANGS_DIR = ROOT / "content" / "languages"

TITLES: dict[str, str] = {
    "advpl": "AdvPL",
    "algol_68c": "ALGOL 68C",
    "algol_68g": "ALGOL 68G",
    "ansi94_forth": "ANSI 94 Forth",
    "ansi_basic": "ANSI BASIC",
    "ansi_standard_basic": "ANSI Standard BASIC",
    "apache_derby": "Apache Derby",
    "applesoft": "Applesoft BASIC",
    "aurelbasic": "AurelBasic",
    "batch": "Batch File",
    "befunge_93": "Befunge-93",
    "c_17": "C++17",
    "c_cli": "C++/CLI",
    "casio_basic": "Casio BASIC",
    "cfscript": "CFScript",
    "cixl": "Cixl",
    "clipper_xbase": "Clipper/xBase",
    "css": "CSS",
    "eleven_l": "11l",
    "esql": "ESQL",
    "excel_vba": "Excel VBA",
    "filemaker": "FileMaker",
    "genexus": "GeneXus",
    "gnu_apl_dyalog_apl": "GNU APL / Dyalog APL",
    "go_2": "Go 2",
    "gridscript": "GridScript",
    "html": "HTML",
    "javafx_script": "JavaFX Script",
    "javascript_nodejs": "JavaScript (Node.js)",
    "jocaml": "JoCaml",
    "json": "JSON",
    "jscript_net": "JScript.NET",
    "lispworks": "LispWorks",
    "lox": "Lox",
    "lse": "L.S.E.",
    "lua_torch": "Lua (Torch)",
    "mapbasic": "MapBasic",
    "masm_basic": "MASM BASIC",
    "metal": "Metal",
    "minizinc": "MiniZinc",
    "mixal": "MIXAL",
    "ml": "ML",
    "mozart_oz": "Mozart/Oz",
    "ms_smallbasic": "Microsoft Small Basic",
    "myhdl": "MyHDL",
    "nasm": "NASM",
    "ncurses": "ncurses",
    "nix": "Nix",
    "nmake_exe": "NMAKE.EXE",
    "oberon": "Oberon",
    "openedge_abl_progress_4gl": "OpenEdge ABL (Progress 4GL)",
    "oracle": "Oracle",
    "os_8_basic": "OS/8 BASIC",
    "postgresql": "PostgreSQL",
    "quack": "Quack",
    "quackasm": "QuackASM",
    "quickbasic": "QuickBASIC",
    "rpgiv": "RPG IV",
    "ruby_with_rspec": "Ruby with RSpec",
    "sac": "SAC",
    "snabel": "Snabel",
    "softbridge_basic": "SoftBridge BASIC",
    "sqlite": "SQLite",
    "svg": "SVG",
    "teradata_stored_procedure": "Teradata Stored Procedure",
    "thinbasic": "thinBasic",
    "ti_basic": "TI BASIC",
    "toffeescript": "ToffeeScript",
    "vb6": "Visual Basic 6",
    "vedit": "Vedit macro language",
    "web_68": "Web 68",
    "webassembly": "WebAssembly",
    "winbatch": "WinBatch",
    "wolframalpha": "Wolfram Alpha",
    "xfractint": "XFractint",
    "xidel": "Xidel",
    "xmidas": "X-Midas",
    "xtalk": "xTalk",
    "xtend": "Xtend",
    "z_arch_assembler": "z/Architecture Assembler",
    "zsh": "Zsh",
}

TEMPLATE = """+++
title = "{title}"
description = ""
aliases = []
[taxonomies]
categories = []
tags = []
+++
"""


def main() -> int:
    created = 0
    skipped = 0
    for slug, title in sorted(TITLES.items()):
        path = LANGS_DIR / f"{slug}.md"
        if path.exists():
            skipped += 1
            continue
        path.write_text(TEMPLATE.format(title=title))
        created += 1
        print(f"  created  {slug}.md  ({title})")
    print(f"\nCreated: {created}, already existed: {skipped}, total: {len(TITLES)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
