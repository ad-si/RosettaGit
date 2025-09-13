+++
title = "Interactive Help"
description = ""
date = 2019-08-30T16:07:09Z
aliases = []
[extra]
id = 22172
[taxonomies]
categories = ["task"]
tags = []
+++

Demonstrate any interactive (or command line) help offered by the language implementation.

This can be for exploring features or syntax, or perhaps a simple message on where to find more information.





## AWK


```AWK

# syntax: GAWK --help

```

```txt

Usage: gawk [POSIX or GNU style options] -f progfile [--] file ...
Usage: gawk [POSIX or GNU style options] [--] 'program' file ...
POSIX options:          GNU long options: (standard)
        -f progfile             --file=progfile
        -F fs                   --field-separator=fs
        -v var=val              --assign=var=val
Short options:          GNU long options: (extensions)
        -b                      --characters-as-bytes
        -c                      --traditional
        -C                      --copyright
        -d[file]                --dump-variables[=file]
        -D[file]                --debug[=file]
        -e 'program-text'       --source='program-text'
        -E file                 --exec=file
        -g                      --gen-pot
        -h                      --help
        -i includefile          --include=includefile
        -l library              --load=library
        -L [fatal]              --lint[=fatal]
        -n                      --non-decimal-data
        -M                      --bignum
        -N                      --use-lc-numeric
        -o[file]                --pretty-print[=file]
        -O                      --optimize
        -p[file]                --profile[=file]
        -P                      --posix
        -r                      --re-interval
        -S                      --sandbox
        -t                      --lint-old
        -V                      --version

To report bugs, see node `Bugs' in `gawk.info', which is
section `Reporting Problems and Bugs' in the printed version.

gawk is a pattern scanning and processing language.
By default it reads standard input and writes standard output.

Examples:
        gawk '{ sum += $1 }; END { print sum }' file
        gawk -F: '{ print $1 }' /etc/passwd

```


## Go

The 'go doc' command line tool shows documentation for either a package or a symbol within a package.

For example, here we list the exported functions and types in the 'strings' package in the standard library:

```txt

$ go doc strings
package strings // import "strings"

Package strings implements simple functions to manipulate UTF-8 encoded
strings.

For information about UTF-8 strings in Go, see
https://blog.golang.org/strings.

func Compare(a, b string) int
func Contains(s, substr string) bool
func ContainsAny(s, chars string) bool
func ContainsRune(s string, r rune) bool
func Count(s, substr string) int
func EqualFold(s, t string) bool
func Fields(s string) []string
func FieldsFunc(s string, f func(rune) bool) []string
func HasPrefix(s, prefix string) bool
func HasSuffix(s, suffix string) bool
func Index(s, substr string) int
func IndexAny(s, chars string) int
func IndexByte(s string, c byte) int
func IndexFunc(s string, f func(rune) bool) int
func IndexRune(s string, r rune) int
func Join(a []string, sep string) string
func LastIndex(s, substr string) int
func LastIndexAny(s, chars string) int
func LastIndexByte(s string, c byte) int
func LastIndexFunc(s string, f func(rune) bool) int
func Map(mapping func(rune) rune, s string) string
func Repeat(s string, count int) string
func Replace(s, old, new string, n int) string
func Split(s, sep string) []string
func SplitAfter(s, sep string) []string
func SplitAfterN(s, sep string, n int) []string
func SplitN(s, sep string, n int) []string
func Title(s string) string
func ToLower(s string) string
func ToLowerSpecial(c unicode.SpecialCase, s string) string
func ToTitle(s string) string
func ToTitleSpecial(c unicode.SpecialCase, s string) string
func ToUpper(s string) string
func ToUpperSpecial(c unicode.SpecialCase, s string) string
func Trim(s string, cutset string) string
func TrimFunc(s string, f func(rune) bool) string
func TrimLeft(s string, cutset string) string
func TrimLeftFunc(s string, f func(rune) bool) string
func TrimPrefix(s, prefix string) string
func TrimRight(s string, cutset string) string
func TrimRightFunc(s string, f func(rune) bool) string
func TrimSpace(s string) string
func TrimSuffix(s, suffix string) string
type Builder struct{ ... }
type Reader struct{ ... }
    func NewReader(s string) *Reader
type Replacer struct{ ... }
    func NewReplacer(oldnew ...string) *Replacer

BUG: The rule Title uses for word boundaries does not handle Unicode punctuation properly.

```

We can now 'dig down' to obtain the documentation for the TrimSpace function wthin the strings package:

```txt

$ go doc strings.TrimSpace
func TrimSpace(s string) string
    TrimSpace returns a slice of the string s, with all leading and trailing
    white space removed, as defined by Unicode.

```

and to list the methods of the strings.Replacer struct:

```txt

$ go doc strings.Replacer
type Replacer struct {
	// Has unexported fields.
}
    Replacer replaces a list of strings with replacements. It is safe for
    concurrent use by multiple goroutines.


func NewReplacer(oldnew ...string) *Replacer
func (r *Replacer) Replace(s string) string
func (r *Replacer) WriteString(w io.Writer, s string) (n int, err error)

```

Finally, we obtain the documentation for the strings.Replacer.WriteString method:

```txt

$ go doc strings.Replacer.WriteString
func (r *Replacer) WriteString(w io.Writer, s string) (n int, err error)
    WriteString writes s to w with all replacements performed.

```

The 'go doc' tool also has some flags including a flag to list both exported and unexported symbols (-u) and to respect case when matching symbols (-c); normally lower- case letters match either case.


## Jsish


```txt
prompt$ jsish
Jsish interactive: see 'help [cmd]'
# help
Jsish interactive executes commands, uses tab for completions, and has help for the following builtin commands:

     Array Boolean CData CEnum CStruct CType Channel Debugger Event File
     Function Info Interp JSON Math Number Object RegExp Signal Socket
     Sqlite String System Util Vfs WebSocket Zvfs assert clearInterval
     console decodeURI encodeURI exec exit format isFinite isMain isNaN
     load log noOp parseFloat parseInt parseOpts printf provide puts quote
     require runModule setInterval setTimeout sleep source strftime strptime
     unload update 

Help can also take options.  For example to display in a web browser try:

    help -web true WebSocket

Module help can also be displayed (non-web), as in 'help Jsi_Websrv`.
Builtin modules include:

     Jsi_Archive Jsi_CData Jsi_Csspp Jsi_Debug Jsi_DebugUI Jsi_GenDeep
     Jsi_Help Jsi_Htmlpp Jsi_Jspp Jsi_Markdeep Jsi_Module Jsi_Safe Jsi_SqliteUI
     Jsi_UnitTest Jsi_Vfs Jsi_Websrv Jsi_Wget
# help File
File.method(...)
Commands for accessing the filesystem
Methods: atime chdir chmod copy dirname executable exists extension glob isdir isfile isrelative join link lstat mkdir mknod mtime owned pwd read readable readlink realpath remove rename rootname size stat tail tempfile truncate type writable write

[File.glob options]
Option          Type    Description [Flags]
----------------------------------------------------------------------------
dir             STRING  The start directory: this path will not be prepended to results.
maxDepth        INT     Maximum directory depth to recurse into.
maxDiscard      INT     Maximum number of items to discard before giving up.
dirFilter       FUNC    Filter function for directories, returning false to discard. @function(dir:string)
filter          FUNC    Filter function to call with each file, returning false to discard. @function(file:string)
limit           INT     The maximum number of results to return/count.
noTypes         STRKEY  Filter files to exclude these "types".
prefix          STRKEY  String prefix to add to each file in list.
recurse         BOOL    Recurse into sub-directories.
retCount        BOOL    Return only the count of matches.
tails           BOOL    Returned only tail of path.
types           STRKEY  Filter files to include type: one or more of chars 'fdlpsbc' for file, directory, link, etc.
```



## Julia

When Julia is run without a program file as argument, the REPL (Read-Evaluate-Print Loop) runs. Entering a ? at the prompt brings up help, with help on a specific topic obtained if the topic name is preceded by ? at the prompt.

```julia


> julia
               _
   _       _ _(_)_     |  Documentation: https://docs.julialang.org
  (_)     | (_) (_)    |
   _ _   _| |_  __ _   |  Type "?" for help, "]?" for Pkg help.
  | | | | | | |/ _` |  |
  | | |_| | | | (_| |  |  Version 1.1.1 (2019-05-16)
 _/ |\__'_|_|_|\__'_|  |  Official https://julialang.org/ release
|__/                   |

help?>
search: ⊻ ⊋ ⊊ ⊉ ⊈ ⊇ ⊆ ≥ ≤ ≢ ≡ ≠ ≉ ≈ ∪ ∩ ∛ √ ∘ ∌ ∋ ∉ ∈ ℯ π ÷ ~ | ^ \ > < : / - + * & % ! if do IO |> rm pi mv in im fd cp

  Welcome to Julia 1.1.1. The full manual is available at

  https://docs.julialang.org/

  as well as many great tutorials and learning resources:

  https://julialang.org/learning/

  For help on a specific function or macro, type ? followed by its name, e.g. ?cos, or ?@time, and press enter. Type ;
  to enter shell mode, ] to enter package mode.

help?> function
search: function Function functionloc @functionloc @cfunction

  function

  Functions are defined with the function keyword:

  function add(a, b)
      return a + b
  end

  Or the short form notation:

  add(a, b) = a + b

  The use of the return keyword is exactly the same as in other languages, but is often optional. A function without
  an explicit return statement will return the last expression in the function body.

```



## Perl


```txt
Usage: perl [switches] [--] [programfile] [arguments]
  -0[octal]         specify record separator (\0, if no argument)
  -a                autosplit mode with -n or -p (splits $_ into @F)
  -C[number/list]   enables the listed Unicode features
  -c                check syntax only (runs BEGIN and CHECK blocks)
  -d[:debugger]     run program under debugger
  -D[number/list]   set debugging flags (argument is a bit mask or alphabets)
  -e program        one line of program (several -e's allowed, omit programfile)
  -E program        like -e, but enables all optional features
  -f                don't do $sitelib/sitecustomize.pl at startup
  -F/pattern/       split() pattern for -a switch (//'s are optional)
  -i[extension]     edit <> files in place (makes backup if extension supplied)
  -Idirectory       specify @INC/#include directory (several -I's allowed)
  -l[octal]         enable line ending processing, specifies line terminator
  -[mM][-]module    execute "use/no module..." before executing program
  -n                assume "while (<>) { ... }" loop around program
  -p                assume loop like -n but print line also, like sed
  -s                enable rudimentary parsing for switches after programfile
  -S                look for programfile using PATH environment variable
  -t                enable tainting warnings
  -T                enable tainting checks
  -u                dump core after parsing program
  -U                allow unsafe operations
  -v                print version, patchlevel and license
  -V[:variable]     print configuration summary (or a single Config.pm variable)
  -w                enable many useful warnings
  -W                enable all warnings
  -x[directory]     ignore text before #!perl line (optionally cd to directory)
  -X                disable all warnings

Run 'perldoc perl' for more help with Perl.
```


```txt
GETTING HELP
    The perldoc program gives you access to all the documentation that comes
    with Perl. You can get more documentation, tutorials and community
    support online at <http://www.perl.org/>.

    If you're new to Perl, you should start by running "perldoc perlintro",
    which is a general intro for beginners and provides some background to
    help you navigate the rest of Perl's extensive documentation. Run
    "perldoc perldoc" to learn more things you can do with perldoc.

    For ease of access, the Perl manual has been split up into several
    sections.

  Overview
        perl                Perl overview (this section)
        perlintro           Perl introduction for beginners
        perlrun             Perl execution and options
        perltoc             Perl documentation table of contents

  Tutorials
        perlreftut          Perl references short introduction
        perldsc             Perl data structures intro
        perllol             Perl data structures: arrays of arrays
        perlrequick         Perl regular expressions quick start
        perlretut           Perl regular expressions tutorial
        perlootut           Perl OO tutorial for beginners
        perlperf            Perl Performance and Optimization Techniques
        perlstyle           Perl style guide
        perlcheat           Perl cheat sheet
        perltrap            Perl traps for the unwary
        perldebtut          Perl debugging tutorial
        perlfaq             Perl frequently asked questions
          perlfaq1          General Questions About Perl
          perlfaq2          Obtaining and Learning about Perl
          perlfaq3          Programming Tools
          perlfaq4          Data Manipulation
          perlfaq5          Files and Formats
          perlfaq6          Regexes
          perlfaq7          Perl Language Issues
          perlfaq8          System Interaction
          perlfaq9          Networking

  Reference Manual
        perlsyn             Perl syntax
        perldata            Perl data structures
        perlop              Perl operators and precedence
        perlsub             Perl subroutines
        perlfunc            Perl built-in functions
        perlpod             Perl plain old documentation
        perldiag            Perl diagnostic messages
        perldeprecation     Perl deprecations
        perllexwarn         Perl warnings and their control
        perldebug           Perl debugging
        perlvar             Perl predefined variables
        perlre              Perl regular expressions, the rest of the story
        perlref             Perl references, the rest of the story
        perlobj             Perl objects
        perltie             Perl objects hidden behind simple variables
        perlipc             Perl interprocess communication
        perlfork            Perl fork() information
        perlnumber          Perl number semantics
        perlthrtut          Perl threads tutorial
        perlport            Perl portability guide
        perllocale          Perl locale support
        perlunicode         Perl Unicode support
        perlunifaq          Perl Unicode FAQ
        perlunitut          Perl Unicode tutorial
        perlsec             Perl security
        perlmod             Perl modules: how they work
        perlmodlib          Perl modules: how to write and use
        perlmodinstall      Perl modules: how to install from CPAN
        perlutil            utilities packaged with the Perl distribution
        perlglossary        Perl Glossary
        perlbook            Perl book information
        perlcommunity       Perl community information
        perldoc             Look up Perl documentation in Pod format
        perldelta           Perl changes since previous version
```



## Perl 6

Perl 6 help is generally in a specialized text format known as POD (Plain Old Documentation). It is sometimes referred to as POD6 to distinguish it from Perl 5 POD which is slightly different and not completely compatible. Perl 6 has a local command line help app: p6doc. It also has online browsable HTML documentation at [https://docs.perl6.org docs.perl6.org]. If you want to download the HTML docs for a local copy, or just prefer to browse the documentation as a single page [https://docs.perl6.org/perl6.html docs.perl6.org/perl6.html] may be more to your preference. If you prefer a different format, there are [https://modules.perl6.org/search/?q=POD%3A%3ATo utilities available] to convert the POD docs to several different formats; Markdown, PDF, Latex, plain text, etc.

Individual Perl 6 scripts are to some extent self-documenting. If the script has a MAIN sub, and it is called with improper parameters, it will display an automatically generated help message showing the various possible parameters, which are required, which are optional, and what type each takes:


```perl6
sub MAIN(
    Str $run,             #= Task or file name
    Str :$lang = 'perl6', #= Language, default perl6
    Int :$skip = 0,       #= Skip # to jump partially into a list
    Bool :f(:$force),     #= Override any skip parameter
) {
    # do whatever
}
```


```txt
Usage:
  main.p6 [--lang=<Str>] [--skip=<Int>] [-f|--force] <run>
  
    <run>           Task or file name
    --lang=<Str>    Language, default perl6
    --skip=<Int>    Skip # to jump partially into a list
    -f|--force      Override any skip parameter

```



## Phix

See docs\phix\phix.chm for the complete manual, or [http://phix.x10.mx/docs/html/phix.htm online].

When p -?, p -h, p -help, or p --help is entered:

```txt

interpret: <filename>
compile: -c <filename>

also: -d creates a list.asm file,
      -nodiag creates a smaller list.asm file,
      -norun may be useful in batch jobs,
      -test, tnn, b, bt, etc, and of course
      -c p, which rebuilds the compiler.
      for more details\options see pth.exw.

Wildcards are allowed, as long as they select a single file,
 with ".exw" assumed, eg "demo\tak*" runs demo\takeuchi.exw.

Press F7 to list prompt history, up/down to select.

Phix hybrid interpreter/compiler.

Version 0.8.0 (32 bit Windows) Copyright Pete Lomax 2006..2016

Enter ? for options or filename to execute:

```

(Pressing ? at that last prompt would re-display the text just shown.)


## Racket


In the command line, <code>raco docs <optional-query></code> will open internet browser and show locally-built Racket docs (main Racket and and any packages locally installed).

In the Racket REPL, <code>help</code> or <code>(help <optional-query>)</code> functions similar to <code>raco docs</code>. Note that if the query is an identifier, <code>help</code> will be able to extract lexical information attached to the identifier, resulting in a precise documentation lookup.

In Racket file, <code>help</code> could be obtained by <code>(require racket/help)</code>


## REXX

Some REXXes offer interactive help   (via '''HELP''' or some other command). 

Other REXXes have an HTML document or a PDF for showing command syntax and other general information on use of the language. 

Each REXX has it's own documentation, and viewing it would depend on the host environment.





## Rust


The Rust compiler's help screen:


```txt

$ rustc --help
Usage: rustc [OPTIONS] INPUT

Options:
    -h, --help          Display this message
        --cfg SPEC      Configure the compilation environment
    -L [KIND=]PATH      Add a directory to the library search path. The
                        optional KIND can be one of dependency, crate, native,
                        framework, or all (the default).
    -l [KIND=]NAME      Link the generated crate(s) to the specified native
                        library NAME. The optional KIND can be one of
                        static, framework, or dylib (the default).
        --crate-type [bin|lib|rlib|dylib|cdylib|staticlib|proc-macro]
                        Comma separated list of types of crates
                        for the compiler to emit
        --crate-name NAME
                        Specify the name of the crate being built
        --edition 2015|2018
                        Specify which edition of the compiler to use when
                        compiling code.
        --emit [asm|llvm-bc|llvm-ir|obj|metadata|link|dep-info|mir]
                        Comma separated list of types of output for the
                        compiler to emit
        --print [crate-name|file-names|sysroot|cfg|target-list|target-cpus|target-features|relocation-models|code-models|tls-models|target-spec-json|native-static-libs]
                        Compiler information to print on stdout
    -g                  Equivalent to -C debuginfo=2
    -O                  Equivalent to -C opt-level=2
    -o FILENAME         Write output to <filename>
        --out-dir DIR   Write output to compiler-chosen filename in <dir>
        --explain OPT   Provide a detailed explanation of an error message
        --test          Build a test harness
        --target TARGET Target triple for which the code is compiled
    -W, --warn OPT      Set lint warnings
    -A, --allow OPT     Set lint allowed
    -D, --deny OPT      Set lint denied
    -F, --forbid OPT    Set lint forbidden
        --cap-lints LEVEL
                        Set the most restrictive lint level. More restrictive
                        lints are capped at this level
    -C, --codegen OPT[=VALUE]
                        Set a codegen option
    -V, --version       Print version info and exit
    -v, --verbose       Use verbose output

Additional help:
    -C help             Print codegen options
    -W help             Print 'lint' options and default settings
    -Z help             Print unstable compiler options
    --help -v           Print the full set of options rustc accepts


```


Cargo's help screen:


```txt

$ cargo --help
Rust's package manager

USAGE:
    cargo [OPTIONS] [SUBCOMMAND]

OPTIONS:
    -V, --version           Print version info and exit
        --list              List installed commands
        --explain <CODE>    Run `rustc --explain CODE`
    -v, --verbose           Use verbose output (-vv very verbose/build.rs output)
    -q, --quiet             No output printed to stdout
        --color <WHEN>      Coloring: auto, always, never
        --frozen            Require Cargo.lock and cache are up to date
        --locked            Require Cargo.lock is up to date
        --offline           Run without accessing the network
    -Z <FLAG>...            Unstable (nightly-only) flags to Cargo, see 'cargo -Z help' for details
    -h, --help              Prints help information

Some common cargo commands are (see all commands with --list):
    build       Compile the current package
    check       Analyze the current package and report errors, but don't build object files
    clean       Remove the target directory
    doc         Build this package's and its dependencies' documentation
    new         Create a new cargo package
    init        Create a new cargo package in an existing directory
    run         Run a binary or example of the local package
    test        Run the tests
    bench       Run the benchmarks
    update      Update dependencies listed in Cargo.lock
    search      Search registry for crates
    publish     Package and upload this package to the registry
    install     Install a Rust binary. Default location is $HOME/.cargo/bin
    uninstall   Uninstall a Rust binary

See 'cargo help <command>' for more information on a specific command.

```

