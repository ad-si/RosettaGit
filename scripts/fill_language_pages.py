#!/usr/bin/env python3
"""Fill in the 81 stub language pages created in the previous step with
short descriptive text, tags where confident, and external links
(Wikipedia / official website) where they exist.

Each entry is a tuple: (tags, extra_fields, body). tags is a list of
paradigm/style keywords matching the conventions used in existing
language pages (functional, interpreted, static, etc.). extra_fields
is a dict of [extra] fields such as website or wikipedia.
"""
from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LANGS_DIR = ROOT / "content" / "languages"


def entry(tags=None, website=None, wikipedia=None, body=""):
    return {
        "tags": tags or [],
        "website": website,
        "wikipedia": wikipedia,
        "body": body.strip(),
    }


PAGES: dict[str, dict] = {
    "advpl": entry(
        body="AdvPL (Advanced Protheus Language) is a proprietary programming "
             "language developed by TOTVS, primarily used to customize and "
             "extend the Protheus / Microsiga ERP system. Its syntax is "
             "derived from xBase."
    ),
    "algol_68c": entry(
        wikipedia="ALGOL_68C",
        body="ALGOL 68C is a dialect of ALGOL 68 developed at the University "
             "of Cambridge. The CHAOS operating system and several early "
             "compilers were written in it."
    ),
    "algol_68g": entry(
        website="https://jmvdveer.home.xs4all.nl/en.algol-68-genie.html",
        body="ALGOL 68 Genie (Algol68G) is a hybrid compiler-interpreter for "
             "full ALGOL 68 written by Marcel van der Veer."
    ),
    "ansi94_forth": entry(
        tags=["stack-based", "imperative"],
        wikipedia="Forth_(programming_language)",
        body="ANSI 94 Forth refers to the ANSI X3.215-1994 standard for the "
             "Forth programming language, later adopted internationally as "
             "ISO/IEC 15145."
    ),
    "ansi_basic": entry(
        tags=["imperative"],
        wikipedia="ANSI_BASIC",
        body="ANSI BASIC is the BASIC programming language as standardised "
             "by ANSI. The minimal subset is ANSI X3.60-1978; the full "
             "standard ANSI X3.113-1987 is also known as ANSI Standard BASIC."
    ),
    "ansi_standard_basic": entry(
        tags=["imperative", "structured"],
        wikipedia="ANSI_BASIC",
        body="ANSI Standard BASIC (ANSI X3.113-1987, ISO/IEC 10279:1991) is "
             "the full standard for the BASIC language. It is a structured "
             "dialect with data types, external modules, and graphics "
             "primitives."
    ),
    "apache_derby": entry(
        tags=["declarative"],
        website="https://db.apache.org/derby/",
        wikipedia="Apache_Derby",
        body="Apache Derby is an open-source relational database "
             "implemented entirely in Java. It is small enough to embed in "
             "applications and ships with the JDK as Java DB."
    ),
    "applesoft": entry(
        tags=["interpreted", "imperative"],
        wikipedia="Applesoft_BASIC",
        body="Applesoft BASIC is the dialect of Microsoft BASIC supplied "
             "with the Apple II computer, replacing the earlier Integer "
             "BASIC. It adds floating-point arithmetic and graphics "
             "commands."
    ),
    "aurelbasic": entry(
        body="AurelBasic is a small BASIC-like interpreter."
    ),
    "batch": entry(
        tags=["imperative", "interpreted"],
        wikipedia="Batch_file",
        body="Batch File (.bat or .cmd) is the scripting language used by "
             "the DOS, OS/2, and Microsoft Windows command-line "
             "interpreters to automate sequences of commands."
    ),
    "befunge_93": entry(
        tags=["esoteric", "stack-based"],
        wikipedia="Befunge",
        body="Befunge-93 is the original 1993 version of Befunge, a "
             "two-dimensional esoteric programming language in which the "
             "instruction pointer moves across a toroidal grid of single-"
             "character instructions."
    ),
    "c_17": entry(
        tags=["static", "compiled", "object-oriented"],
        wikipedia="C%2B%2B17",
        body="C++17 is the 2017 revision of the ISO/IEC C++ standard. It "
             "introduced structured bindings, `if constexpr`, "
             "`std::optional`, `std::variant`, `std::filesystem`, and many "
             "other features."
    ),
    "c_cli": entry(
        tags=["static", "compiled", "object-oriented"],
        wikipedia="C%2B%2B/CLI",
        body="C++/CLI is a Microsoft-designed variant of C++ for the Common "
             "Language Infrastructure, replacing Managed C++ and enabling "
             "interoperability between native and managed .NET code."
    ),
    "casio_basic": entry(
        tags=["interpreted", "imperative"],
        body="Casio BASIC is a family of BASIC dialects built into Casio "
             "graphing calculators and pocket computers, used for "
             "programming calculator-side utilities."
    ),
    "cfscript": entry(
        tags=["interpreted", "dynamic"],
        wikipedia="ColdFusion_Markup_Language",
        body="CFScript is the JavaScript-inspired scripting syntax of "
             "Adobe ColdFusion and Lucee. It is an alternative to the "
             "tag-based CFML syntax for writing server-side web "
             "applications."
    ),
    "cixl": entry(
        tags=["stack-based", "dynamic"],
        body="Cixl is an extensible stack-based, dynamically typed "
             "scripting language with first-class symbols, records, "
             "optionals, coroutines, and persistent data structures."
    ),
    "clipper_xbase": entry(
        wikipedia="Clipper_(programming_language)",
        body="Clipper is a dBase-based programming language in the xBase "
             "family, originally developed by Nantucket Corporation for "
             "compiling dBase III applications on DOS."
    ),
    "css": entry(
        tags=["declarative"],
        website="https://www.w3.org/Style/CSS/",
        wikipedia="CSS",
        body="CSS (Cascading Style Sheets) is a stylesheet language for "
             "describing the presentation of HTML and XML documents, "
             "including layout, colour, and typography."
    ),
    "eleven_l": entry(
        tags=["static", "compiled"],
        website="https://11l-lang.org/",
        body="11l is a statically typed programming language designed "
             "for easy translation from Python. It compiles to efficient "
             "C++."
    ),
    "esql": entry(
        body="ESQL (Extended Structured Query Language) is a proprietary "
             "SQL-based programming language used by IBM Integration Bus / "
             "App Connect Enterprise for message-flow transformation "
             "logic."
    ),
    "excel_vba": entry(
        tags=["interpreted", "object-oriented"],
        wikipedia="Visual_Basic_for_Applications",
        body="Excel VBA is Visual Basic for Applications as hosted within "
             "Microsoft Excel. It is used to automate spreadsheets, build "
             "user-defined functions, and create macros."
    ),
    "filemaker": entry(
        wikipedia="FileMaker",
        body="FileMaker is a cross-platform relational database "
             "application from Claris. It integrates a database engine "
             "with a graphical interface and includes a scripting "
             "language for workflow automation."
    ),
    "genexus": entry(
        website="https://www.genexus.com/",
        wikipedia="GeneXus",
        body="GeneXus is a knowledge-based, declarative development tool "
             "produced by Globant/Artech that generates application code "
             "for multiple platforms from high-level business-object "
             "models."
    ),
    "gnu_apl_dyalog_apl": entry(
        tags=["array", "functional"],
        wikipedia="APL_(programming_language)",
        body="GNU APL and Dyalog APL are two implementations of APL — an "
             "array-oriented programming language originally designed by "
             "Kenneth Iverson. GNU APL is a free, faithful implementation "
             "of ISO 13751; Dyalog APL is a widely used commercial "
             "implementation."
    ),
    "go_2": entry(
        website="https://go.dev/",
        body="Go 2 is the informal name for the evolution of the Go "
             "programming language after Go 1, comprising changes proposed "
             "under the “Go 2 transition” such as generics, error-handling "
             "improvements, and modules."
    ),
    "gridscript": entry(
        body="GridScript is a scripting language."
    ),
    "html": entry(
        tags=["markup"],
        website="https://html.spec.whatwg.org/",
        wikipedia="HTML",
        body="HTML (HyperText Markup Language) is the standard markup "
             "language for documents displayed in a web browser. It "
             "describes the structure of web pages using elements "
             "represented by tags."
    ),
    "javafx_script": entry(
        wikipedia="JavaFX_Script",
        body="JavaFX Script was a declarative, statically typed scripting "
             "language that shipped with JavaFX 1.x for writing rich-"
             "client interfaces on top of the Java runtime. Oracle "
             "dropped it in favour of the JavaFX Java API."
    ),
    "javascript_nodejs": entry(
        tags=["interpreted", "dynamic", "event-driven"],
        website="https://nodejs.org/",
        wikipedia="Node.js",
        body="JavaScript running on Node.js — a V8-based server-side "
             "JavaScript runtime. Node.js provides non-blocking I/O and a "
             "large package ecosystem (npm) for building network "
             "applications."
    ),
    "jocaml": entry(
        tags=["functional", "concurrent", "static"],
        wikipedia="JoCaml",
        body="JoCaml is an experimental functional programming language "
             "derived from OCaml that integrates the join-calculus with "
             "concurrent programming primitives."
    ),
    "json": entry(
        tags=["data-format"],
        website="https://www.json.org/",
        wikipedia="JSON",
        body="JSON (JavaScript Object Notation) is a lightweight data-"
             "interchange format derived from JavaScript object literal "
             "syntax. It represents simple values, arrays, and key/value "
             "objects as plain text."
    ),
    "jscript_net": entry(
        tags=["interpreted", "object-oriented"],
        wikipedia="JScript_.NET",
        body="JScript .NET is Microsoft's ECMAScript-compatible "
             "programming language for the .NET platform. It adds types, "
             "classes, and packages to the dynamic base language."
    ),
    "lispworks": entry(
        website="https://www.lispworks.com/",
        body="LispWorks is a commercial Common Lisp implementation and "
             "integrated development environment produced by LispWorks "
             "Ltd. It includes a GUI toolkit, editor, debugger, and "
             "compiler."
    ),
    "lox": entry(
        tags=["dynamic", "interpreted", "object-oriented"],
        website="https://craftinginterpreters.com/",
        body="Lox is a dynamic, object-oriented scripting language "
             "designed by Robert Nystrom as the teaching vehicle in his "
             "book *Crafting Interpreters*."
    ),
    "lse": entry(
        wikipedia="LSE_(programming_language)",
        body="L.S.E. (Langage Symbolique d'Enseignement) is an educational "
             "programming language created in 1971 at the École Supérieure "
             "d'Électricité in France."
    ),
    "lua_torch": entry(
        tags=["interpreted", "dynamic"],
        website="http://torch.ch/",
        wikipedia="Torch_(machine_learning)",
        body="Torch is a scientific computing framework for Lua with wide "
             "support for machine-learning algorithms and GPU computation. "
             "It was widely used for deep-learning research before PyTorch."
    ),
    "mapbasic": entry(
        wikipedia="MapBasic",
        body="MapBasic is a BASIC-based scripting language for the MapInfo "
             "Professional GIS application. It is used to extend and "
             "automate map-based workflows."
    ),
    "masm_basic": entry(
        body="MASM BASIC is a BASIC-like scripting dialect implemented on "
             "top of Microsoft Macro Assembler (MASM) via macros."
    ),
    "metal": entry(
        wikipedia="Metal_(API)",
        body="Metal is Apple's low-level graphics and compute API, "
             "combining functionality similar to OpenGL and OpenCL. It "
             "includes the Metal Shading Language, based on C++14."
    ),
    "minizinc": entry(
        tags=["declarative", "constraint"],
        website="https://www.minizinc.org/",
        wikipedia="MiniZinc",
        body="MiniZinc is a declarative, high-level constraint modelling "
             "language used to specify constraint-satisfaction and "
             "optimisation problems independently of any particular solver."
    ),
    "mixal": entry(
        tags=["assembly"],
        wikipedia="MIX_(abstract_machine)",
        body="MIXAL is the assembly language for Donald Knuth's "
             "hypothetical MIX computer, used throughout the first editions "
             "of *The Art of Computer Programming*."
    ),
    "ml": entry(
        tags=["functional", "static"],
        wikipedia="ML_(programming_language)",
        body="ML (Meta Language) is a general-purpose functional "
             "programming language developed at the University of "
             "Edinburgh, known for its strong static type system with "
             "type inference and pattern matching. Major dialects "
             "include Standard ML and OCaml."
    ),
    "mozart_oz": entry(
        tags=["multi-paradigm", "declarative", "concurrent"],
        website="http://mozart2.org/",
        wikipedia="Oz_(programming_language)",
        body="Oz is a multi-paradigm programming language developed by "
             "the Mozart Consortium. It supports functional, logic, "
             "object-oriented, constraint, distributed, and concurrent "
             "programming within one uniform framework."
    ),
    "ms_smallbasic": entry(
        wikipedia="Microsoft_Small_Basic",
        body="Microsoft Small Basic is a simplified programming language "
             "and environment for learners, derived from BASIC. It "
             "provides a small object-oriented standard library aimed at "
             "teaching programming fundamentals."
    ),
    "myhdl": entry(
        website="https://www.myhdl.org/",
        wikipedia="MyHDL",
        body="MyHDL is an open-source Python package for hardware "
             "description and verification. It allows Python to be used "
             "both for writing synthesisable hardware and for testbenches."
    ),
    "nasm": entry(
        tags=["assembly"],
        website="https://nasm.us/",
        wikipedia="Netwide_Assembler",
        body="NASM (Netwide Assembler) is a portable x86/x86-64 assembler "
             "for Intel syntax with support for many output formats "
             "including ELF, Mach-O, and COFF."
    ),
    "ncurses": entry(
        website="https://www.gnu.org/software/ncurses/",
        wikipedia="Ncurses",
        body="ncurses (new curses) is a library for writing portable "
             "terminal-based user interfaces on character-cell terminals."
    ),
    "nix": entry(
        tags=["functional", "declarative", "lazy", "pure"],
        website="https://nixos.org/",
        wikipedia="Nix_(package_manager)",
        body="Nix is a purely functional, lazily evaluated domain-specific "
             "language for specifying package builds and system "
             "configurations in the Nix package manager and NixOS."
    ),
    "nmake_exe": entry(
        wikipedia="Make_(software)",
        body="NMAKE.EXE is Microsoft's implementation of *make*, a build-"
             "automation tool that reads a makefile to determine and "
             "execute build steps."
    ),
    "oberon": entry(
        tags=["imperative", "structured", "static"],
        wikipedia="Oberon_(programming_language)",
        body="Oberon is a general-purpose programming language created by "
             "Niklaus Wirth in 1986 as the successor to Modula-2. It "
             "emphasises simplicity and extensibility."
    ),
    "openedge_abl_progress_4gl": entry(
        wikipedia="OpenEdge_Advanced_Business_Language",
        body="OpenEdge ABL (formerly Progress 4GL) is a fourth-generation, "
             "SQL-like language developed by Progress Software for "
             "building business applications against OpenEdge databases."
    ),
    "oracle": entry(
        tags=["declarative"],
        wikipedia="Oracle_Database",
        body="Oracle is a proprietary multi-model relational database "
             "management system produced by Oracle Corporation. It is "
             "programmed using SQL and the PL/SQL procedural extension."
    ),
    "os_8_basic": entry(
        tags=["interpreted", "imperative"],
        wikipedia="OS/8",
        body="OS/8 BASIC is the BASIC implementation bundled with Digital "
             "Equipment Corporation's OS/8 operating system for the PDP-8 "
             "minicomputer."
    ),
    "postgresql": entry(
        tags=["declarative"],
        website="https://www.postgresql.org/",
        wikipedia="PostgreSQL",
        body="PostgreSQL is a free, open-source relational database "
             "management system emphasising extensibility and SQL "
             "compliance. Its procedural extensions (PL/pgSQL, PL/Python, "
             "and others) let developers write stored procedures and "
             "functions."
    ),
    "quack": entry(
        body="Quack is a small programming language."
    ),
    "quackasm": entry(
        tags=["assembly"],
        body="QuackASM is an assembler-style language used in the Quack "
             "programming ecosystem."
    ),
    "quickbasic": entry(
        tags=["imperative", "structured"],
        wikipedia="QuickBASIC",
        body="QuickBASIC is a structured variant of Microsoft BASIC for "
             "MS-DOS and classic Mac OS, released in 1985. It added "
             "subroutines, structured control flow, and a compiled form."
    ),
    "rpgiv": entry(
        wikipedia="IBM_RPG",
        body="RPG IV (formally ILE RPG) is the fourth major revision of "
             "IBM's Report Program Generator programming language for the "
             "AS/400 / IBM i platform. It adds integrated-language-"
             "environment features to the original RPG."
    ),
    "ruby_with_rspec": entry(
        tags=["dynamic", "interpreted", "object-oriented"],
        website="https://rspec.info/",
        body="Ruby with RSpec refers to the combination of Ruby and the "
             "RSpec behaviour-driven-development testing framework, which "
             "provides a domain-specific language for writing executable "
             "specifications."
    ),
    "sac": entry(
        tags=["functional", "data-parallel"],
        wikipedia="SAC_programming_language",
        body="SAC (Single-Assignment C) is a data-parallel functional "
             "programming language with C-like syntax aimed at numerical "
             "array programming and automatic parallelisation."
    ),
    "snabel": entry(
        tags=["stack-based", "concatenative", "static"],
        body="Snabel is a concatenative, statically typed programming "
             "language implemented as a C++ library."
    ),
    "softbridge_basic": entry(
        body="SoftBridge BASIC (SBB) is a Microsoft-Basic-compatible "
             "scripting language formerly produced by SoftBridge Inc. for "
             "integrating desktop office applications."
    ),
    "sqlite": entry(
        tags=["declarative"],
        website="https://www.sqlite.org/",
        wikipedia="SQLite",
        body="SQLite is a small, fast, self-contained, public-domain "
             "embedded SQL database engine. Its library is included in "
             "many platforms, browsers, and mobile devices."
    ),
    "svg": entry(
        tags=["markup", "declarative"],
        website="https://www.w3.org/Graphics/SVG/",
        wikipedia="Scalable_Vector_Graphics",
        body="SVG (Scalable Vector Graphics) is an XML-based vector "
             "image format for two-dimensional graphics, with support for "
             "interactivity and animation."
    ),
    "teradata_stored_procedure": entry(
        wikipedia="Teradata",
        body="Teradata Stored Procedure (SPL — Stored Procedure Language) "
             "is the procedural extension to Teradata SQL, used to write "
             "stored procedures for the Teradata Database."
    ),
    "thinbasic": entry(
        tags=["interpreted"],
        website="https://www.thinbasic.com/",
        body="thinBasic is an open-source, interpreted BASIC-like "
             "scripting language for Windows. It focuses on rapid "
             "application development with built-in GUI, graphics, and "
             "database modules."
    ),
    "ti_basic": entry(
        tags=["interpreted", "imperative"],
        wikipedia="TI-BASIC",
        body="TI-BASIC is a family of BASIC dialects built into Texas "
             "Instruments calculators and some early TI microcomputers."
    ),
    "toffeescript": entry(
        tags=["interpreted", "dynamic"],
        body="ToffeeScript is a CoffeeScript-inspired language that adds "
             "asynchronous error-handling and continuation-passing "
             "syntax."
    ),
    "vb6": entry(
        tags=["interpreted", "compiled", "object-oriented"],
        wikipedia="Visual_Basic_(classic)",
        body="Visual Basic 6.0 is the final version of the classic Visual "
             "Basic programming language, released by Microsoft in 1998. "
             "It was widely used for Windows desktop applications and was "
             "succeeded by Visual Basic .NET."
    ),
    "vedit": entry(
        website="https://www.vedit.com/",
        body="Vedit macro language is the scripting language of the Vedit "
             "text editor. It is used to automate editing tasks with "
             "pattern-based search-and-replace, regions, and control flow."
    ),
    "web_68": entry(
        wikipedia="WEB",
        body="Web 68 is an ALGOL 68 port of Donald Knuth's WEB literate-"
             "programming system. It lets authors write programs that "
             "combine ALGOL 68 code with typeset documentation."
    ),
    "webassembly": entry(
        tags=["stack-based", "compiled"],
        website="https://webassembly.org/",
        wikipedia="WebAssembly",
        body="WebAssembly (Wasm) is a binary instruction format for a "
             "stack-based virtual machine, designed as a portable "
             "compilation target for high-level languages to run on the "
             "web at near-native speed."
    ),
    "winbatch": entry(
        website="https://www.winbatch.com/",
        body="WinBatch is a Windows-based batch-automation scripting "
             "language produced by Wilson WindowWare."
    ),
    "wolframalpha": entry(
        website="https://www.wolframalpha.com/",
        body="Wolfram|Alpha is a computational knowledge engine developed "
             "by Wolfram Research that answers factual queries by "
             "computing answers from curated data."
    ),
    "xfractint": entry(
        website="https://fractint.org/",
        body="Xfractint is an X11 port of the Fractint fractal-generating "
             "program. Its scripting facilities allow users to program "
             "fractal algorithms with a small declarative formula "
             "language."
    ),
    "xidel": entry(
        website="https://videlibri.sourceforge.net/xidel.html",
        body="Xidel is a command-line tool for downloading and extracting "
             "data from HTML, XML, and JSON pages using XPath, XQuery, "
             "and CSS selectors."
    ),
    "xmidas": entry(
        website="https://www.xmidas.org/",
        body="X-Midas is a digital signal processing package originally "
             "developed at the US Naval Research Laboratory. It is used "
             "for building, prototyping, and deploying signal-processing "
             "pipelines."
    ),
    "xtalk": entry(
        wikipedia="XTalk",
        body="xTalk is the family of English-like, weakly typed scripting "
             "languages derived from HyperCard's HyperTalk, used in "
             "authoring environments such as SuperCard and Revolution / "
             "LiveCode."
    ),
    "xtend": entry(
        tags=["static", "object-oriented", "functional"],
        website="https://eclipse.dev/Xtext/xtend/",
        wikipedia="Xtend",
        body="Xtend is a general-purpose, statically typed programming "
             "language that compiles to Java source code. It runs on the "
             "JVM and focuses on concise, functional-leaning Java-interop."
    ),
    "z_arch_assembler": entry(
        tags=["assembly"],
        wikipedia="IBM_High_Level_Assembler",
        body="z/Architecture Assembler is the assembly language for IBM's "
             "z/Architecture mainframes, successor to the S/390 and "
             "System/370 assembler. It is typically assembled by IBM's "
             "High Level Assembler (HLASM)."
    ),
    "zsh": entry(
        tags=["shell", "interpreted"],
        website="https://www.zsh.org/",
        wikipedia="Z_shell",
        body="Zsh is a Unix shell that extends the Bourne shell with "
             "interactive features from bash, ksh, and tcsh, including "
             "powerful globbing, history, and completion. It has been "
             "the default shell on macOS since Catalina."
    ),
}


def render(title: str, data: dict) -> str:
    tags = data["tags"]
    website = data["website"]
    wikipedia = data["wikipedia"]
    body = data["body"]

    tags_block = "[]"
    if tags:
        tags_block = "[\n" + "".join(f'  "{t}",\n' for t in tags) + "]"

    extra_lines = []
    if website:
        extra_lines.append(f'website = "{website}"')
    if wikipedia:
        extra_lines.append(f'wikipedia = "{wikipedia}"')
    extra_block = "[extra]\n" + "\n".join(extra_lines) + "\n" if extra_lines else ""

    return (
        "+++\n"
        f'title = "{title}"\n'
        'description = ""\n'
        "aliases = []\n"
        f"{extra_block}"
        "[taxonomies]\n"
        "categories = []\n"
        f"tags = {tags_block}\n"
        "+++\n"
        "\n"
        f"{body}\n"
    )


def main() -> int:
    # Titles from the creation step
    from create_language_pages import TITLES

    missing_data = sorted(set(TITLES) - set(PAGES))
    extra_data = sorted(set(PAGES) - set(TITLES))
    if missing_data:
        print(f"WARN: no body for: {missing_data}")
    if extra_data:
        print(f"WARN: body for non-stub slug: {extra_data}")

    written = 0
    for slug, data in sorted(PAGES.items()):
        title = TITLES[slug]
        path = LANGS_DIR / f"{slug}.md"
        path.write_text(render(title, data))
        written += 1
    print(f"Wrote {written} pages.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
