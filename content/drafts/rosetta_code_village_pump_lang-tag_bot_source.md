+++
title = "Rosetta Code:Village Pump/Lang-tag bot/Source"
description = ""
date = 2011-09-08T08:11:41Z
aliases = []
[extra]
id = 4977
[taxonomies]
categories = []
tags = []
+++


```perl
use warnings;
use strict;
use feature 'say';
use List::Util '&minstr';
use HTML::Entities '&decode_entities';
use YAML::XS qw(&DumpFile &LoadFile);
use MediaWiki::API;

use constant DELAY_BETWEEN_EDITS => 10 * 60;    # In seconds.

my $username = 'UnderBot';
my $password = 'secret';

my $kill_switch_page    = 'User talk:UnderBot';
my $kill_switch_trigger = qr/stopediting/;

my $tasks_path = '/home/hippo/Temporary/tasks.yml';

my %langtags = ('4d'                      => '4d',
                'actionscript'            => 'actionscript',
                'ada'                     => 'ada',
                'agda2'                   => 'agda2',
                'algol 60'                => 'algol60',
                'algol 68'                => 'algol68',
                'amigae'                  => 'amigae',
                'apl'                     => 'apl',
                'applescript'             => 'applescript',
                'assembly'                => 'asm',
                'autohotkey'              => 'autohotkey',
                'awk'                     => 'awk',
                'bc'                      => 'bc',
                'befunge'                 => 'befunge',
                'Brainfuck'               => 'bf',
                'caml'                    => 'caml',
                'c'                       => 'c',
                'c++'                     => 'cpp',
                'clean'                   => 'clean',
                'clojure'                 => 'lisp',
                'cobol'                   => 'cobol',
                'coldfusion'              => 'cfm',
                'common lisp'             => 'lisp',
                'component pascal'        => 'pascal',
                'coq'                     => 'coq',
                'c sharp|c#'              => 'csharp',
                'c sharp'                 => 'csharp',
                'dc'                      => 'dc',
                'd'                       => 'd',
                'delphi'                  => 'delphi',
                'dos batch file'          => 'dos',
                'ec'                      => 'ec',
                'e'                       => 'e',
                'eiffel'                  => 'eiffel',
                'ella'                    => 'ella',
                'emacs lisp'              => 'lisp',
                'erlang'                  => 'erlang',
                'esql'                    => 'sql',
                'factor'                  => 'factor',
                'false'                   => 'false',
                'fan'                     => 'fan',
                'f'                       => 'f',
                'forth'                   => 'forth',
                'fortran'                 => 'fortran',
                'fp'                      => 'fp',
                'f sharp|f#'              => 'fsharp',
                'f_sharp|f#'              => 'fsharp',
                'gap'                     => 'gap',
                'genyris'                 => 'genyris',
                'gnuplot'                 => 'gnuplot',
                'go'                      => 'go',
                'groovy'                  => 'groovy',
                'haskell'                 => 'haskell',
                'haxe'                    => 'haxe',
                'hq9+'                    => 'hq9p',
                'html'                    => 'html4strict',
                'icon'                    => 'icon',
                'idl'                     => 'idl',
                'io'                      => 'io',
                'javafx script'           => 'javafx',
                'java'                    => 'java',
                'javascript'              => 'javascript',
                'j'                       => 'j',
                'jocaml'                  => 'jocaml',
                'joy'                     => 'joy',
                'jscript.net'             => 'jscript.net',
                'json'                    => 'json',
                'judoscript'              => 'judoscript',
                'korn shell'              => 'korn',
                'labview'                 => 'labview',
                'latex'                   => 'latex',
                'lisaac'                  => 'lisaac',
                'lisp'                    => 'lisp',
                'logo'                    => 'logo',
                'logtalk'                 => 'logtalk',
                'lotusscript'             => 'lotusscript',
                'lse64'                   => 'lse64',
                'lua'                     => 'lua',
                'lucid'                   => 'lucid',
                'm4'                      => 'm4',
                'make'                    => 'make',
                'maple'                   => 'maple',
                'mathematica'             => 'mathematica',
                'matlab'                  => 'matlab',
                'maxima'                  => 'maxima',
                'maxscript'               => 'maxscript',
                'metafont'                => 'metafont',
                'mirc scripting language' => 'mirc',
                'mmix'                    => 'mmix',
                'modula-2'                => 'modula2',
                'modula-3'                => 'modula3',
                'moo'                     => 'moo',
                'mpif90'                  => 'mpif90',
                'ms sql'                  => 'sql',
                'mysql'                   => 'sql',
                'newlisp'                 => 'lisp',
                'nial'                    => 'nial',
                'oberon-2'                => 'oberon2',
                'objective-c'             => 'objc',
                'object pascal'           => 'objectpascal',
                'ocaml'                   => 'ocaml',
                'octave'                  => 'octave',
                'omega'                   => 'omega',
                'openedge/progress'       => 'openedge',
                'oz'                      => 'oz',
                'pari/gp'                 => 'parigp',
                'pascal'                  => 'pascal',
                'perl 6'                  => 'perl6',
                'perl'                    => 'perl',
                'php'                     => 'php',
                'pike'                    => 'pike',
                'plaintex'                => 'tex',
                'pl/i'                    => 'pli',
                'pl/pgsql'                => 'plpgsql',
                'pl/sql'                  => 'plsql',
                'pop11'                   => 'pop11',
                'postgresql'              => 'sql',
                'postscript'              => 'postscript',
                'powerbasic'              => 'powerbasic',
                'powershell'              => 'powershell',
                'prolog'                  => 'prolog',
                'pure'                    => 'pure',
                'python'                  => 'python',
                'q'                       => 'q',
                'rapidq'                  => 'rapidq',
                'raven'                   => 'raven',
                'rexx'                    => 'rexx',
                'rhope'                   => 'rhope',
                'r'                       => 'r',
                'ruby'                    => 'ruby',
                'sas'                     => 'sas',
                'scala'                   => 'scala',
                'scheme'                  => 'scheme',
                'script3d'                => 'script3d',
                'seed7'                   => 'seed7',
                'self'                    => 'self',
                'setl'                    => 'setl',
                'slate'                   => 'slate',
                'smalltalk'               => 'smalltalk',
                'smeql'                   => 'smeql',
                'snusp'                   => 'snusp',
                'sql'                     => 'sql',
                'standard ml'             => 'sml',
                'supercollider'           => 'supercollider',
                'svg'                     => 'xml',
                'tcl'                     => 'tcl',
                'ti-83 basic'             => 'ti83b',
                'ti-89 basic'             => 'ti89b',
                'toka'                    => 'toka',
                'transact-sql'            => 'sql',
                'tr'                      => 'tr',
                'twelf'                   => 'twelf',
                'unixpipes'               => 'bash',
                'unix shell'              => 'bash',
                'unlambda'                => 'unlambda',
                'ursala'                  => 'ursala',
                'vbscript'                => 'vbscript',
                'vedit macro language'    => 'vedit',
                'visual basic .net'       => 'vbnet',
                'visual basic'            => 'vb',
                'visual objects'          => 'visobj',
                'vorpal'                  => 'vorpal',
                'v'                       => 'v',
                'wrapl'                   => 'wrapl',
                'xquery'                  => 'xquery',
                'xslt'                    => 'xml',
                'xtalk'                   => 'xtalk',);

my $h = qr/(?:\t| )*/;

# Vaguely like Perl 6's \h.
my $lwsl = qr/(?:\t| )+\S[^\n]*/;

# Leading WhiteSpace Line.

# ------------------------------------------------------------

our (%tasks, @done, @todo);
local *tasks = LoadFile $tasks_path;
local *done  = $tasks{done};
local *todo  = $tasks{todo};

my $mw = new MediaWiki::API({api_url => 'http://rosettacode.org/mw/api.php'});
$mw->login({lgname => $username, lgpassword => $password})
  or die q{Couldn't log in. (}, $mw->{error}->{code}, ': ',
  $mw->{error}->{details}, ')';

while (@todo) {
    my $pagetitle = shift @todo;
    say "TITLE: $pagetitle";

    # Check the kill switch.
    my $p = $mw->get_page({title => $kill_switch_page}) || die;
    $p->{'*'} =~ $kill_switch_trigger and die "Killed.\n";

    $p = $mw->get_page({title => $pagetitle}) || die;
    my $timestamp = $p->{timestamp};    # To prevent edit conflicts.
    my $text      = $p->{'*'};

    $text =~ s/ (.+? \n) (== \s* {{) /$2/xs or die;
    my $newtext = $1;

    # So $newtext just contains the task description so far.
    while (
        $text =~ s! \A
             ( == $h {{ $h header $h \| $h ([^}]+?) $h }} $h == $h \n )

             (.+?)

             ( \z | == $h {{ )

              !$4!xs
      ) {
        my ($header, $langname, $body) = ($1, $2, $3);
        s/\bC #/C#/ foreach $header, $langname;

        # Why some people put a space there, I have no idea.
        my $tag =
             $langtags{lc $langname}
          || $langname =~ /assembl/i && 'asm'
          ||
          ## BASIC dialect-guessing is commented out because
          ## on some pages, programs for more than one dialect
          ## appear under "BASIC". Really we ought to treat
          ## each dialect as its own language.
          #$langname =~ /basic/i &&
          #   ($body =~ /q(uick)?basic/i && 'qbasic' ||
          #    $body =~ /f(ree)?basic/i && 'freebasic' ||
          #    $body =~ /t(hin)?basic/i && 'thinbasic') ||
          undef;

        if ($tag) {
            $tag = "<lang $tag>";
            if ($body =~ /<lang/) {    # Use the correct identifier.
                $body =~ s {$h (<lang [^>]* >)}
                   {my $s = $1;
                    lc($s) eq $tag || $s =~ /java5/i
                    ? $s # Don't replace "Mathematica" with "mathematica" or "java5" with "java"
                    : $tag}gxe;

                # Get rid of any indenting spaces left behind when
                # someone else added the lang tags.
                lc($langname) eq 'whitespace' or $body =~ s
                    {(<lang [^>]* >) ((?:$h\n)*) (.+?) \s* \x3c/lang>}
                    {my ($t, $leading, $b) = ($1, $2, $3);
                     if ($b !~ /^\S/m and ($leading or $b =~ /\n/))
                       # It there's no newline in $b, the
                       # indentation is probably intentional (as
                       # in many J examples).
                        {my $space = minstr($b =~ /^( +)\S/gm);
                         $b =~ s/^$space//gm;}
                      "$t$b\x3c/lang>"}xges;
            }
            elsif ($body =~ /<pre/)

              # Just assume they should all be lang tags.
            {
                $body =~ s
                    { <pre [^>]* > \s*
                      (.+?)
                      \s*
```
 }
                    {decode_entities "$tag$1\x3c/lang>"}xseg;
            }

            # HTML entities don't work in lang tags.
            # But they aren't necessary, either.
            else

              # Turn indented passages into lang-tagged passages.
            {
                $body =~ s
                   { (  ^ $lwsl \n
                      (?: (?: $lwsl \n | $h \n )*
                           $lwsl \n )? ) }
                   {my $t = $1;
                    my $space = minstr($t =~ /^( +)\S/gm);
                    $t =~ s/^$space//gm;
                    $t =~ s/\s+\z//;
                    decode_entities("$tag$t\x3c/lang>\n");}mgex;
            }
        }

        $body =~ s
            {(<lang [^>]*>) <nowiki> \s* (.+?) \s* </nowiki> \x3c/lang>}
            {$1$2\x3c/lang>}gsx;
        $newtext .= $header . $body;
    }

    $newtext .= $text;
    $newtext =~ s/\s*\z/\n/;

    my $success = $mw->edit(
        {
         action        => 'edit',
         title         => $pagetitle,
         basetimestamp => $timestamp,
         text          => $newtext,
         minor         => 1,

         # All we're doing, ultimately, is formatting.
         bot      => 1,
         nocreate => 1,

         # If the page was deleted while we were regexing,
         # we probably shouldn't resurrect it!
         summary => 'Fixed lang tags.'
        },
        {skip_encoding => 1});

    # Without the skip_encoding option, non-ASCII characters
    # will get corrupted.

    if ($success) {
        say(exists $success->{edit}->{nochange}
            ? 'Unchanged.'
            : 'Committed!');
        push @done, $pagetitle;
    }
    else

      # Probably an edit conflict.
    {
        say "Couldn't commit; I'll try again later.";
        push @todo, $pagetitle;
    }
    DumpFile $tasks_path, \%tasks;

    sleep DELAY_BETWEEN_EDITS;
}
```


[[Category:Perl]]
