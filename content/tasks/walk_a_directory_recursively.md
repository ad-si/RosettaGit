+++
title = "Walk a directory/Recursively"
description = ""
date = 2019-09-07T17:23:38Z
aliases = []
[extra]
id = 1851
[taxonomies]
categories = ["task", "File System Operations"]
tags = []
+++

## Task

Walk a given directory ''tree'' and print files matching a given pattern.


'''Note:''' This task is for recursive methods.   These tasks should read an entire directory tree, not a ''single directory''.


'''Note:''' Please be careful when running any code examples found here.


## Related tasks

*   [[Walk a directory/Non-recursively]]   (read a ''single directory'').





## 8th


```forth

"*.c" f:rglob \ top of stack now has list of all "*.c" files, recursively

```



## Ada


```ada
with Ada.Directories;  use Ada.Directories;
with Ada.Text_IO;

procedure Test_Directory_Walk is
   procedure Walk (Name : String; Pattern : String) is
      procedure Print (Item : Directory_Entry_Type) is
      begin
         Ada.Text_IO.Put_Line (Full_Name (Item));
      end Print;
      procedure Walk (Item : Directory_Entry_Type) is
      begin
         if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
            Walk (Full_Name (Item), Pattern);
         end if;
      exception
         when Name_Error => null;
      end Walk;
   begin
      Search (Name, Pattern, (others => True), Print'Access);
      Search (Name, "", (Directory => True, others => False), Walk'Access);
   end Walk;
begin
   Walk (".", "*.adb");
end Test_Directory_Walk;
```

The solution first enumerates files in a directory, that includes the subdirectories, if their names match the pattern. Then it steps down into each of the subdirectories. The pseudo directories . and .. are excluded. The behavior upon symbolic links depends on the [[OS]] and the implementation of the Ada.Directories package.


## ALGOL 68

<!-- {{does not work with|ALGOL 68|Standard - extensions to language used}} -->
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - ''get directory'' and'' grep in string'' not available in any library ... yet}} -->

```algol68
INT match=0, no match=1, out of memory error=2, other error=3;

STRING slash = "/", pwd=".", parent="..";

PROC walk tree = (STRING path, PROC (STRING)VOID call back)VOID: (
  []STRING files = get directory(path);
  FOR file index TO UPB files DO
    STRING file = files[file index];
    STRING path file = path+slash+file;
    IF file is directory(path file) THEN
      IF file NE pwd AND file NE parent THEN
        walk tree(path file, call back)
      FI
    ELSE
      call back(path file)
    FI
  OD
);

STRING re sort a68 = "[Ss]ort[^/]*[.]a68$";

PROC match sort a68 and print = (STRING path file)VOID:
  IF grep in string(re sort a68, path file, NIL, NIL) = match THEN
    print((path file, new line))
  FI;

walk tree(".", match sort a68 and print)
```

```txt

./Shell_sort_c.a68
./Quick_sort.a68
./Shell_sort.a68
./Cocktail_Sort.a68
./Selection_Sort.a68
./Merge_sort.a68
./tmp/test_sort.a68
./Bobosort.a68
./Sorting_an_Array_of_Integers.a68
./Insertion_Sort.a68
./Permutation_Sort.a68

```



## AutoHotkey

Display all TMP files in Temp directory and its subdirectories.

```autohotkey
Loop, %A_Temp%\*.tmp,,1
 out .= A_LoopFileName "`n"
MsgBox,% out
```



## BaCon

This line will recursively walk though all directories starting from the current directory ".":

```qbasic
PRINT WALK$(".", 1, ".+", TRUE, NL$)
```



## Batch File

A sample code that displays all the EXE files in System32 directory recursively.

```dos
dir /s /b "%windir%\System32\*.exe"
```

----
If you wanted to apply some command to each item in a directory tree, then use <code>FOR</code> with the switch <code>/R</code>. For example, to apply the ECHO command to every DLL file in C:\Windows\System32:
```dos
FOR /R C:\Windows\System32 %%F IN (*.DLL) DO ECHO "%%F"
```

This can be done from outside a batch file (entered directly at the command prompt) by changing the double percent signs (%%) to single percents (%):

```dos
FOR /R C:\Windows\System32 %F IN (*.DLL) DO ECHO "%F"
```



## BBC BASIC

```bbcbasic
      directory$ = "C:\Windows\"
      pattern$ = "*.chm"
      PROClisttree(directory$, pattern$)
      END

      DEF PROClisttree(dir$, filter$)
      LOCAL dir%, sh%, res%
      DIM dir% LOCAL 317
      IF RIGHT$(dir$) <> "\" IF RIGHT$(dir$) <> "/" dir$ += "\"
      SYS "FindFirstFile", dir$ + filter$, dir% TO sh%
      IF sh% <> -1 THEN
        REPEAT
          IF (!dir% AND 16) = 0 PRINT dir$ + $$(dir%+44)
          SYS "FindNextFile", sh%, dir% TO res%
        UNTIL res% = 0
        SYS "FindClose", sh%
      ENDIF
      SYS "FindFirstFile", dir$ + "*", dir% TO sh%
      IF sh% <> -1 THEN
        REPEAT
          IF (!dir% AND 16) IF dir%?44 <> &2E THEN
            PROClisttree(dir$ + $$(dir%+44) + "\", filter$)
          ENDIF
          SYS "FindNextFile", sh%, dir% TO res%
        UNTIL res% = 0
        SYS "FindClose", sh%
      ENDIF
      ENDPROC
```



## C

==={{libheader|POSIX}}===
```C>#include <sys/types.h

#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <regex.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <err.h>

enum {
	WALK_OK = 0,
	WALK_BADPATTERN,
	WALK_NAMETOOLONG,
	WALK_BADIO,
};

#define WS_NONE		0
#define WS_RECURSIVE	(1 << 0)
#define WS_DEFAULT	WS_RECURSIVE
#define WS_FOLLOWLINK	(1 << 1)	/* follow symlinks */
#define WS_DOTFILES	(1 << 2)	/* per unix convention, .file is hidden */
#define WS_MATCHDIRS	(1 << 3)	/* if pattern is used on dir names too */

int walk_recur(char *dname, regex_t *reg, int spec)
{
	struct dirent *dent;
	DIR *dir;
	struct stat st;
	char fn[FILENAME_MAX];
	int res = WALK_OK;
	int len = strlen(dname);
	if (len >= FILENAME_MAX - 1)
		return WALK_NAMETOOLONG;

	strcpy(fn, dname);
	fn[len++] = '/';

	if (!(dir = opendir(dname))) {
		warn("can't open %s", dname);
		return WALK_BADIO;
	}

	errno = 0;
	while ((dent = readdir(dir))) {
		if (!(spec & WS_DOTFILES) && dent->d_name[0] == '.')
			continue;
		if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, ".."))
			continue;

		strncpy(fn + len, dent->d_name, FILENAME_MAX - len);
		if (lstat(fn, &st) == -1) {
			warn("Can't stat %s", fn);
			res = WALK_BADIO;
			continue;
		}

		/* don't follow symlink unless told so */
		if (S_ISLNK(st.st_mode) && !(spec & WS_FOLLOWLINK))
			continue;

		/* will be false for symlinked dirs */
		if (S_ISDIR(st.st_mode)) {
			/* recursively follow dirs */
			if ((spec & WS_RECURSIVE))
				walk_recur(fn, reg, spec);

			if (!(spec & WS_MATCHDIRS)) continue;
		}

		/* pattern match */
		if (!regexec(reg, fn, 0, 0, 0)) puts(fn);
	}

	if (dir) closedir(dir);
	return res ? res : errno ? WALK_BADIO : WALK_OK;
}

int walk_dir(char *dname, char *pattern, int spec)
{
	regex_t r;
	int res;
	if (regcomp(&r, pattern, REG_EXTENDED | REG_NOSUB))
		return WALK_BADPATTERN;
	res = walk_recur(dname, &r, spec);
	regfree(&r);

	return res;
}

int main()
{
	int r = walk_dir(".", ".\\.c$", WS_DEFAULT|WS_MATCHDIRS);
	switch(r) {
	case WALK_OK:		break;
	case WALK_BADIO:	err(1, "IO error");
	case WALK_BADPATTERN:	err(1, "Bad pattern");
	case WALK_NAMETOOLONG:	err(1, "Filename too long");
	default:
		err(1, "Unknown error?");
	}
	return 0;
}
```

==={{libheader|BSD libc}}===
With the [http://www.openbsd.org/cgi-bin/man.cgi?query=fts&apropos=0&sektion=3&manpath=OpenBSD+Current&arch=i386&format=html fts(3)] functions from 4.4BSD, this program can sort the files, and can also detect cycles (when a link puts a directory inside itself). This program makes a ''logical traversal'' that follows symbolic links to directories.
```c
#include <sys/types.h>
#include <err.h>
#include <errno.h>
#include <fnmatch.h>
#include <fts.h>
#include <string.h>
#include <stdio.h>

/* Compare files by name. */
int
entcmp(const FTSENT **a, const FTSENT **b)
{
	return strcmp((*a)->fts_name, (*b)->fts_name);
}

/*
 * Print all files in the directory tree that match the glob pattern.
 * Example: pmatch("/usr/src", "*.c");
 */
void
pmatch(char *dir, const char *pattern)
{
	FTS *tree;
	FTSENT *f;
	char *argv[] = { dir, NULL };

	/*
	 * FTS_LOGICAL follows symbolic links, including links to other
	 * directories. It detects cycles, so we never have an infinite
	 * loop. FTS_NOSTAT is because we never use f->statp. It uses
	 * our entcmp() to sort files by name.
	 */
	tree = fts_open(argv, FTS_LOGICAL | FTS_NOSTAT, entcmp);
	if (tree == NULL)
		err(1, "fts_open");

	/*
	 * Iterate files in tree. This iteration always skips
	 * "." and ".." because we never use FTS_SEEDOT.
	 */
	while ((f = fts_read(tree))) {
		switch (f->fts_info) {
		case FTS_DNR:	/* Cannot read directory */
		case FTS_ERR:	/* Miscellaneous error */
		case FTS_NS:	/* stat() error */
			/* Show error, then continue to next files. */
			warn("%s", f->fts_path);
			continue;
		case FTS_DP:
			/* Ignore post-order visit to directory. */
			continue;
		}

		/*
		 * Check if name matches pattern. If so, then print
		 * path. This check uses FNM_PERIOD, so "*.c" will not
		 * match ".invisible.c".
		 */
		if (fnmatch(pattern, f->fts_name, FNM_PERIOD) == 0)
			puts(f->fts_path);

		/*
		 * A cycle happens when a symbolic link (or perhaps a
		 * hard link) puts a directory inside itself. Tell user
		 * when this happens.
		 */
		if (f->fts_info == FTS_DC)
			warnx("%s: cycle in directory tree", f->fts_path);
	}

	/* fts_read() sets errno = 0 unless it has error. */
	if (errno != 0)
		err(1, "fts_read");

	if (fts_close(tree) < 0)
		err(1, "fts_close");
}

int
main()
{
	pmatch(".", "*.c");
	return 0;
}
```

=== [[Windows]] ===
```c
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

/* Print "message: last Win32 error" to stderr. */
void
oops(const wchar_t *message)
{
	wchar_t *buf;
	DWORD error;

	buf = NULL;
	error = GetLastError();
	FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER |
	    FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
	    NULL, error, 0, (wchar_t *)&buf, 0, NULL);

	if (buf) {
		fwprintf(stderr, L"%ls: %ls", message, buf);
		LocalFree(buf);
	} else {
		/* FormatMessageW failed. */
		fwprintf(stderr, L"%ls: unknown error 0x%x\n",
		    message, error);
	}
}

/*
 * Print all files in a given directory tree that match a given wildcard
 * pattern.
 */
int
main()
{
	struct stack {
		wchar_t			*path;
		size_t			 pathlen;
		size_t			 slashlen;
		HANDLE			 ffh;
		WIN32_FIND_DATAW	 ffd;
		struct stack		*next;
	} *dir, dir0, *ndir;
	size_t patternlen;
	int argc;
	wchar_t **argv, *buf, c, *pattern;

	/* MinGW never provides wmain(argc, argv). */
	argv = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (argv == NULL) {
		oops(L"CommandLineToArgvW");
		exit(1);
	}

	if (argc != 3) {
		fwprintf(stderr, L"usage: %ls dir pattern\n", argv[0]);
		exit(1);
	}

	dir0.path = argv[1];
	dir0.pathlen = wcslen(dir0.path);
	pattern = argv[2];
	patternlen = wcslen(pattern);

	if (patternlen == 0 ||
	    wcscmp(pattern, L".") == 0 ||
	    wcscmp(pattern, L"..") == 0 ||
	    wcschr(pattern, L'/') ||
	    wcschr(pattern, L'\\')) {
		fwprintf(stderr, L"%ls: invalid pattern\n", pattern);
		exit(1);
	}

	/*
	 * Must put backslash between path and pattern, unless
	 * last character of path is slash or colon.
	 *
	 *   'dir' => 'dir\*'
	 *   'dir\' => 'dir\*'
	 *   'dir/' => 'dir/*'
	 *   'c:' => 'c:*'
	 *
	 * 'c:*' and 'c:\*' are different files!
	 */
	c = dir0.path[dir0.pathlen - 1];
	if (c == ':' || c == '/' || c == '\\')
		dir0.slashlen = dir0.pathlen;
	else
		dir0.slashlen = dir0.pathlen + 1;

	/* Allocate space for path + backslash + pattern + \0. */
	buf = calloc(dir0.slashlen + patternlen + 1, sizeof buf[0]);
	if (buf == NULL) {
		perror("calloc");
		exit(1);
	}
	dir0.path = wmemcpy(buf, dir0.path, dir0.pathlen + 1);

	dir0.ffh = INVALID_HANDLE_VALUE;
	dir0.next = NULL;
	dir = &dir0;

	/* Loop for each directory in linked list. */
loop:
	while (dir) {
		/*
		 * At first visit to directory:
		 *   Print the matching files. Then, begin to find
		 *   subdirectories.
		 *
		 * At later visit:
		 *   dir->ffh is the handle to find subdirectories.
		 *   Continue to find them.
		 */
		if (dir->ffh == INVALID_HANDLE_VALUE) {
			/* Append backslash + pattern + \0 to path. */
			dir->path[dir->pathlen] = '\\';
			wmemcpy(dir->path + dir->slashlen,
			    pattern, patternlen + 1);

			/* Find all files to match pattern. */
			dir->ffh = FindFirstFileW(dir->path, &dir->ffd);
			if (dir->ffh == INVALID_HANDLE_VALUE) {
				/* Check if no files match pattern. */
				if (GetLastError() == ERROR_FILE_NOT_FOUND)
					goto subdirs;

				/* Bail out from other errors. */
				dir->path[dir->pathlen] = '\0';
				oops(dir->path);
				goto popdir;
			}

			/* Remove pattern from path; keep backslash. */
			dir->path[dir->slashlen] = '\0';

			/* Print all files to match pattern. */
			do {
				wprintf(L"%ls%ls\n",
				    dir->path, dir->ffd.cFileName);
			} while (FindNextFileW(dir->ffh, &dir->ffd) != 0);
			if (GetLastError() != ERROR_NO_MORE_FILES) {
				dir->path[dir->pathlen] = '\0';
				oops(dir->path);
			}
			FindClose(dir->ffh);

subdirs:
			/* Append * + \0 to path. */
			dir->path[dir->slashlen] = '*';
			dir->path[dir->slashlen + 1] = '\0';

			/* Find first possible subdirectory. */
			dir->ffh = FindFirstFileExW(dir->path,
			    FindExInfoStandard, &dir->ffd,
			    FindExSearchLimitToDirectories, NULL, 0);
			if (dir->ffh == INVALID_HANDLE_VALUE) {
				dir->path[dir->pathlen] = '\0';
				oops(dir->path);
				goto popdir;
			}
		} else {
			/* Find next possible subdirectory. */
			if (FindNextFileW(dir->ffh, &dir->ffd) == 0)
				goto closeffh;
		}

		/* Enter subdirectories. */
		do {
			const wchar_t *fn = dir->ffd.cFileName;
			const DWORD attr = dir->ffd.dwFileAttributes;
			size_t buflen, fnlen;

			/*
			 * Skip '.' and '..', because they are links to
			 * the current and parent directories, so they
			 * are not subdirectories.
			 *
			 * Skip any file that is not a directory.
			 *
			 * Skip all reparse points, because they might
			 * be symbolic links. They might form a cycle,
			 * with a directory inside itself.
			 */
			if (wcscmp(fn, L".") == 0 ||
			    wcscmp(fn, L"..") == 0 ||
			    (attr & FILE_ATTRIBUTE_DIRECTORY) == 0 ||
			    (attr & FILE_ATTRIBUTE_REPARSE_POINT))
				continue;

			ndir = malloc(sizeof *ndir);
			if (ndir == NULL) {
				perror("malloc");
				exit(1);
			}

			/*
			 * Allocate space for path + backslash +
			 *     fn + backslash + pattern + \0.
			 */
			fnlen = wcslen(fn);
			buflen = dir->slashlen + fnlen + patternlen + 2;
			buf = calloc(buflen, sizeof buf[0]);
			if (buf == NULL) {
				perror("malloc");
				exit(1);
			}

			/* Copy path + backslash + fn + \0. */
			wmemcpy(buf, dir->path, dir->slashlen);
			wmemcpy(buf + dir->slashlen, fn, fnlen + 1);

			/* Push dir to list. Enter dir. */
			ndir->path = buf;
			ndir->pathlen = dir->slashlen + fnlen;
			ndir->slashlen = ndir->pathlen + 1;
			ndir->ffh = INVALID_HANDLE_VALUE;
			ndir->next = dir;
			dir = ndir;
			goto loop; /* Continue outer loop. */
		} while (FindNextFileW(dir->ffh, &dir->ffd) != 0);
closeffh:
		if (GetLastError() != ERROR_NO_MORE_FILES) {
			dir->path[dir->pathlen] = '\0';
			oops(dir->path);
		}
		FindClose(dir->ffh);

popdir:
		/* Pop dir from list, free dir, but never free dir0. */
		free(dir->path);
		if (ndir = dir->next)
			free(dir);
		dir = ndir;
	}

	return 0;
}
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace RosettaRecursiveDirectory
{
    class Program
    {
        static IEnumerable<FileInfo> TraverseDirectory(string rootPath, Func<FileInfo, bool> Pattern)
        {
            var directoryStack = new Stack<DirectoryInfo>();
            directoryStack.Push(new DirectoryInfo(rootPath));
            while (directoryStack.Count > 0)
            {
                var dir = directoryStack.Pop();
                try
                {
                    foreach (var i in dir.GetDirectories())
                        directoryStack.Push(i);
                }
                catch (UnauthorizedAccessException) {
                    continue; // We don't have access to this directory, so skip it
                }
                foreach (var f in dir.GetFiles().Where(Pattern)) // "Pattern" is a function
                    yield return f;
            }
        }
        static void Main(string[] args)
        {
            // Print the full path of all .wmv files that are somewhere in the C:\Windows directory or its subdirectories
            foreach (var file in TraverseDirectory(@"C:\Windows", f => f.Extension == ".wmv"))
                Console.WriteLine(file.FullName);
            Console.WriteLine("Done.");
        }
    }
}

```



## C++

```cpp
#include "boost/filesystem.hpp"
#include "boost/regex.hpp"
#include <iostream>

using namespace boost::filesystem;

int main()
{
  path current_dir("."); //
  boost::regex pattern("a.*"); // list all files starting with a
  for (recursive_directory_iterator iter(current_dir), end;
       iter != end;
       ++iter)
  {
    std::string name = iter->path().filename().string();
    if (regex_match(name, pattern))
      std::cout << iter->path() << "\n";
  }
}
```


```cpp

#include <filesystem>
#include <iostream>

namespace fs = std::filesystem;

int main() {
  fs::path current_dir(".");
  // list all files containing an mp3 extension
  for (auto &file : fs::recursive_directory_iterator(current_dir)) {
    if (file.path().extension() == ".mp3")
      std::cout << file.path().filename().string() << std::endl;
  }
}
```


=={{header|Caché ObjectScript}}==


```cos

Class Utils.File [ Abstract ]
{

ClassMethod WalkTree(pDir As %String = "", pMask As %String = "*.*") As %Status
{
	// do some validation
	If pDir="" Quit $$$ERROR($$$GeneralError, "No directory specified.")

	// search input directory for files matching wildcard
	Set fs=##class(%ResultSet).%New("%File.FileSet")
	Set sc=fs.Execute(pDir, pMask)
	While (fs.Next()) {
		Write !, fs.Name
		// sub-directory
		If fs.Type="D" Set sc=..WalkTree(fs.Name, pMask)
	}

	// finished
	Quit $$$OK
}

}

```


```txt

USER>Do ##class(Utils.File).WalkTree("/Swsetup/")

C:\Swsetup\Monitors
C:\Swsetup\Monitors\HP_w2207_3.0
C:\Swsetup\Monitors\HP_w2207_3.0\Files
C:\Swsetup\Monitors\HP_w2207_3.0\Files\HP_w2207.cat
C:\Swsetup\Monitors\HP_w2207_3.0\Files\HP_w2207.icm
C:\Swsetup\Monitors\HP_w2207_3.0\Files\HP_w2207.inf
C:\Swsetup\Monitors\HP_w2207_3.0\HP Display Installer.exe
C:\Swsetup\Monitors\HP_w2207_3.0\HPx64DRV.exe
C:\Swsetup\Monitors\HP_w2207_3.0\Readme.txt

```



## Clojure

The standard function ''file-seq'' does a tree walk.

```clojure
(use '[clojure.java.io])

(defn walk [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (file dirpath)))))

(map #(println (.getPath %)) (walk "src" #".*\.clj"))

```



## CoffeeScript

```coffeescript
fs = require 'fs'

walk = (dir, f_match, f_visit) ->
  _walk = (dir) ->
    fns = fs.readdirSync dir
    for fn in fns
      fn = dir + '/' + fn
      if f_match fn
        f_visit fn
      if fs.statSync(fn).isDirectory()
        _walk fn
  _walk(dir)

dir = '..'
matcher = (fn) -> fn.match /\.coffee/
action = console.log
walk dir, matcher, action
```



## Common Lisp

This example uses the <code>CL-FAD</code> library to achieve compatibility where the ANSI CL standard leaves ambiguities about pathnames. Quicklisp is used to ensure it is loaded. Traversal is depth-first unless <code>:depth-first-p nil</code> is passed.

```lisp
(ql:quickload :cl-fad)
(defun mapc-directory-tree (fn directory &key (depth-first-p t))
  (dolist (entry (cl-fad:list-directory directory))
    (unless depth-first-p
      (funcall fn entry))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-directory-tree fn entry))
    (when depth-first-p
      (funcall fn entry))))

```



```lisp
CL-USER> (mapc-directory-tree (lambda (x)
                                (when (equal (pathname-type x) "lisp")
                                  (write-line (namestring x))))
                              "lang/")
/home/sthalik/lang/lisp/.#bitmap.lisp
/home/sthalik/lang/lisp/avg.lisp
/home/sthalik/lang/lisp/bitmap.lisp
/home/sthalik/lang/lisp/box-muller.lisp
/home/sthalik/lang/lisp/displaced-subseq.lisp
[...]
```



## D


```d
void main() {
    import std.stdio, std.file;

    // Recursive breadth-first scan (use SpanMode.depth for
    // a depth-first scan):
    dirEntries("", "*.d", SpanMode.breadth).writeln;
}
```



## E


```e
def walkTree(directory, pattern) {
  for name => file in directory {
    if (name =~ rx`.*$pattern.*`) {
      println(file.getPath())
    }
    if (file.isDirectory()) {
      walkTree(file, pattern)
    }
  }
}
```

```e
? walkTree(<file:/usr/share/man>, "rmdir")
/usr/share/man/man1/rmdir.1
/usr/share/man/man2/rmdir.2
```



## Elixir


```elixir
defmodule Walk_directory do
  def recursive(dir \\ ".") do
    Enum.each(File.ls!(dir), fn file ->
      IO.puts fname = "#{dir}/#{file}"
      if File.dir?(fname), do: recursive(fname)
    end)
  end
end

Walk_directory.recursive
```


```txt

./check.exs
./e.bat
./foo
./foo/bar
./foo/bar/1
./foo/bar/2
./foo/bar/a
./foo/bar/b
./input.txt
./test.beam
./test.exs
./test.txt

```



## Emacs Lisp


```lisp>ELISP
 (directory-files-recursively "/tmp/el" "\\.el$")
("/tmp/el/1/c.el" "/tmp/el/a.el" "/tmp/el/b.el")
```



## Erlang

Use builtin function filelib:fold_files/5

```txt

10> filelib:fold_files( "/tmp", ".*", true, fun(File, Acc) -> [File|Acc] end, []).
["/tmp/clearcase_inst/local.conf", "/tmp/.X0-lock","/tmp/.cron-check-4000-was-here",
 "/tmp/kerneloops.XyN0SP","/tmp/npicagwD7tf"]
11> filelib:fold_files( "/tmp", ".*\.conf", true, fun(File, Acc) -> [File|Acc] end, []).
["/tmp/clearcase_inst/local.conf"]


```


=={{header|F_Sharp|F#}}==
This code is tail-recursive and lazy.

```fsharp
open System.IO

let rec getAllFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
              yield! getAllFiles d pattern }

getAllFiles "c:\\temp" "*.xml"
|> Seq.iter (printfn "%s")
```



## Factor


```factor
USE: io.directories.search

"." t [
    dup ".factor" tail? [ print ] [ drop ] if
] each-file
```



## Forth

```forth

require unix/filestat.fs
require unix/libc.fs

: $append ( from len to -- )   2DUP >R >R  COUNT + SWAP MOVE  R> R@ C@ + R> C! ;

defer ls-filter

: dots? ( name len -- ? )   drop c@ [char] . = ;

file-stat buffer: statbuf

: isdir ( addr u -- flag )
    statbuf lstat ?ior  statbuf st_mode w@ S_IFMT and S_IFDIR = ;

: (ls-r) ( dir len -- )
  pad c@ >r  pad $append  s" /" pad $append
  pad count open-dir if  drop  r> pad c!  exit  then  ( dirid)
  begin
    dup pad count + 256 rot read-dir throw
  while
    pad count + over dots? 0= if   \ ignore all hidden names
      dup pad count rot + 2dup ls-filter if
        cr 2dup type
      then
      isdir if
        pad count + swap recurse
      else drop then
    else drop then
  repeat
  drop  r> pad c!
  close-dir throw
;

: ls-r ( dir len -- )  0 pad c!  (ls-r) ;

: c-files ( str len -- ? )
  dup 3 < if 2drop false exit then
  + 1- dup c@ 32 or
   dup [char] c <> swap [char] h <> and if drop false exit then
  1- dup c@ [char] . <> if drop false exit then
  drop true ;
' c-files is ls-filter

: all-files ( str len -- ? )   2drop true ;
' all-files is ls-filter

s" ." ls-r cr

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=f48f8d5c2e2e85a8f80bcdc0124a35a5 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sTemp As String

For Each sTemp In RDir("/etc", "*.d")
  Print sTemp
Next

End
```

Output:

```txt

sysctl.d
systemd/ntp-units.d
pam.d
security/limits.d
security/namespace.d
insserv.conf.d
udev/hwdb.d
....

```



## GAP


```gap
Walk := function(name, op)
	local dir, file, e;
	dir := Directory(name);
	for e in SortedList(DirectoryContents(name)) do
		file := Filename(dir, e);
		if IsDirectoryPath(file) then
			if not (e in [".", ".."]) then
				Walk(file, op);
			fi;
		else
			op(file);
		fi;
	od;
end;

# This will print filenames
Walk(".", Display);
```



## Go


```go
package main

import (
    "fmt"
    "os"
    "path/filepath"
)

func VisitFile(fp string, fi os.FileInfo, err error) error {
    if err != nil {
        fmt.Println(err) // can't walk here,
        return nil       // but continue walking elsewhere
    }
    if fi.IsDir() {
        return nil // not a file.  ignore.
    }
    matched, err := filepath.Match("*.mp3", fi.Name())
    if err != nil {
        fmt.Println(err) // malformed pattern
        return err       // this is fatal.
    }
    if matched {
        fmt.Println(fp)
    }
    return nil
}

func main() {
    filepath.Walk("/", VisitFile)
}
```



## Groovy

Search text files in current directory tree in a depth-first fashion:


```groovy
new File('.').eachFileRecurse {
  if (it.name =~ /.*\.txt/) println it;
}
```


Shorter variant:

```groovy
new File('.').eachFileRecurse ~/.*\.txt/, { println it }
```


Variant processing only files:

```groovy
new File('.').eachFileRecurse FILES, ~/.*\.txt/, { println it }
```


Flexible search, traversal can be adapted by providing various options in the options Map, see documentation of method:
[http://docs.groovy-lang.org/latest/html/groovy-jdk/java/io/File.html#traverse(java.util.Map,%20groovy.lang.Closure) traverse(Map options, Closure closure)]

```groovy
new File('.').traverse(
    type         : FILES,
    nameFilter   : ~/.*\.txt/,
    preDir       : { if (it.name == '.svn') return SKIP_SUBTREE },
) { println it }

```



## GUISS

Here we list all files that match the pattern m*.txt in "My Documents" and all of its subfolders:

```guiss
Start,Find,Files and Folders,Dropdown: Look in>My Documents,
Inputbox: filename>m*.txt,Button:Search
```



## Haskell


Using the packages [http://hackage.haskell.org/package/directory-1.2.5.0/docs/System-Directory.html#v:getCurrentDirectory directory] and [https://hackage.haskell.org/package/filemanip-0.3.6.3/docs/System-FilePath-Find.html#v:find filemanip]


```haskell
import System.Environment
import System.Directory
import System.FilePath.Find

search pat dir =
  find always (fileName ~~? pat) dir

main = do [pat] <- getArgs
          dir   <- getCurrentDirectory
          files <- search pat dir
          mapM_ putStrLn files
```

or more classic way:

```haskell
import System.FilePath.Posix
import System.Directory
import System.IO

dir_walk :: FilePath -> (FilePath -> IO ()) -> IO ()
dir_walk top filefunc = do
  isDirectory <- doesDirectoryExist top
  if isDirectory
    then
      do
        files <- listDirectory top
        mapM_ (\file -> dir_walk (top </> file) filefunc) files
    else
      filefunc top

main :: IO ()
main = do
         hSetEncoding stdout utf8
         hSetEncoding stdin  utf8
         let worker fname =
              do if (takeExtension fname == ".hs")
                   then putStrLn fname
                   else return ()
         dir_walk "." worker
```


== Icon and Unicon ==
=
## Icon
=
Icon doesn't support 'stat' or 'open' of a directory; however, information can be obtained by use of the <code>system</code> function to access command line.
=
## Unicon
=

```Unicon

###########################
#  A sequential solution  #
###########################

procedure main()
every write(!getdirs("."))  # writes out all directories from the current directory down
end

procedure getdirs(s)  #: return a list of directories beneath the directory 's'
local D,d,f

if ( stat(s).mode ? ="d" ) & ( d := open(s) ) then {
      D := [s]
      while f := read(d) do
         if not ( ".." ? =f ) then          # skip . and ..
            D |||:= getdirs(s || "/" ||f)
      close(d)
      return D
      }
end

#########################
#  A threaded solution  #
#########################

import threads

global n,           # number of the concurrently running threads
       maxT,        # Max number of concurrent threads ("soft limit")
       tot_threads  # the total number of threads created in the program

procedure main(argv)
   target := argv[1] | stop("Usage: tdir [dir name] [#threads]. #threads default to 2* the number of cores in the machine.")
   tot_threads := n := 1
   maxT := ( integer(argv[2])|
	    (&features? if ="CPU cores " then cores := integer(tab(0)) * 2) | # available cores * 2
   	    4) # default to 4 threads
   t := milliseconds()
   L := getdirs(target)  # writes out all directories from the current directory down
   write((*\L)| 0, " directories in ", milliseconds() - t,
	           " ms using ", maxT, "-concurrent/", tot_threads, "-total threads" )
end

procedure getdirs(s)  # return a list of directories beneath the directory 's'
local D,d,f, thrd

if ( stat(s).mode ? ="d" ) & ( d := open(s) ) then {
      D := [s]
      while f := read(d) do
         if not ( ".." ? =f ) then          # skip . and ..
            if n>=maxT then # max thread count reached
               D |||:= getdirs(s || "/" ||f)
            else # spawn a new thread for this directory
	       {/thrd:=[]; n +:= 1; put(thrd, thread getdirs(s || "/" ||f))}

      close(d)

      if \thrd then{  # If I have threads, collect their results
         tot_threads +:= *thrd
         n -:= 1      # allow new threads to be spawned while I'm waiting/collecting results
	 every wait(th := !thrd) do { # wait for the thread to finish
	    n -:= 1
	    D |||:= <@th   # If the thread produced a result, it is going to be
	                   # stored in its "outbox", <@th in this case serves as
	                   # a deferred return since the thread was created by
			   # thread getdirs(s || "/" ||f)
	                   # this is similar to co-expression activation semantics
            }
         n +:= 1
         }
      return D
      }
end
```



## IDL


```idl
result = file_search( directory, '*.txt', count=cc )
```

This will descend down the directory/ies in the variable <tt>"directory"</tt> (which can be an array) returning an array of strings with the names of the files matching "*.txt" and placing the total number of matches into the variable <tt>"cc"</tt>


## J


```j
require 'dir'
>{."1 dirtree '*.html'
```


The verb <tt>dirtree</tt> returns a file listing of a directory tree as a boxed matrix with file names in the first column. The primitives <tt>>{."1</tt> will return the unboxed contents of the first column.

'*.html' can be replaced by another pattern, of course.


## Java

Done using no pattern. But with end string comparison which gave better results.

```java
import java.io.File;

public class MainEntry {
    public static void main(String[] args) {
        walkin(new File("/home/user")); //Replace this with a suitable directory
    }

    /**
     * Recursive function to descend into the directory tree and find all the files
     * that end with ".mp3"
     * @param dir A file object defining the top directory
     **/
    public static void walkin(File dir) {
        String pattern = ".mp3";

        File listFile[] = dir.listFiles();
        if (listFile != null) {
            for (int i=0; i<listFile.length; i++) {
                if (listFile[i].isDirectory()) {
                    walkin(listFile[i]);
                } else {
                    if (listFile[i].getName().endsWith(pattern)) {
                        System.out.println(listFile[i].getPath());
                    }
                }
            }
        }
    }
}
```

Luckily, <code>java.nio.file.Files</code> gives us a <code>walkFileTree</code> method that does exactly what this task calls for.

```java5
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

public class WalkTree {
	public static void main(String[] args) throws IOException {
		Path start = FileSystems.getDefault().getPath("/path/to/file");
		Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
			@Override
			public FileVisitResult visitFile(Path file,
					BasicFileAttributes attrs) throws IOException {
				if (file.toString().endsWith(".mp3")) {
					System.out.println(file);
				}
				return FileVisitResult.CONTINUE;
			}
		});
	}
}
```

```java
import java.io.IOException;
import java.nio.file.*;

public class WalkTree {
	public static void main(String[] args) throws IOException {
		Path start = FileSystems.getDefault().getPath("/path/to/file");
		Files.walk(start)
		     .filter( path -> path.toFile().isFile())
		     .filter( path -> path.toString().endsWith(".mp3"))
		     .forEach( System.out::println );
	}
}

```



## JavaScript

```javascript
var fso = new ActiveXObject("Scripting.FileSystemObject");

function walkDirectoryTree(folder, folder_name, re_pattern) {
    WScript.Echo("Files in " + folder_name + " matching '" + re_pattern + "':");
    walkDirectoryFilter(folder.files, re_pattern);

    var subfolders = folder.SubFolders;
    WScript.Echo("Folders in " + folder_name + " matching '" + re_pattern + "':");
    walkDirectoryFilter(subfolders, re_pattern);

    WScript.Echo();
    var en = new Enumerator(subfolders);
    while (! en.atEnd()) {
        var subfolder = en.item();
        walkDirectoryTree(subfolder, folder_name + "/" + subfolder.name, re_pattern);
        en.moveNext();
    }
}

function walkDirectoryFilter(items, re_pattern) {
    var e = new Enumerator(items);
    while (! e.atEnd()) {
        var item = e.item();
        if (item.name.match(re_pattern))
            WScript.Echo(item.name);
        e.moveNext();
    }
}

walkDirectoryTree(dir, dir.name, '\\.txt$');
```



## Julia

```julia
rootpath = "/home/user/music"
pattern  = r".mp3$"

for (root, dirs, files) in walkdir(rootpath)
    for file in files
        if occursin(pattern, file) println(file) end
    end
end
```



## Kotlin


```scala
// version 1.2.0

import java.io.File

fun walkDirectoryRecursively(dirPath: String, pattern: Regex): Sequence<String> {
    val d = File(dirPath)
    require (d.exists() && d.isDirectory())
    return d.walk().map { it.name }.filter { it.matches(pattern) }.sorted().distinct() }

fun main(args: Array<String>) {
    val r = Regex("""^v(a|f).*\.h$""")  // get all C header files beginning with 'va' or 'vf'
    val files = walkDirectoryRecursively("/usr/include", r)
    for (file in files) println(file)
}

```


Output (Ubuntu 14.04):

```txt

valarray_after.h
valarray_array.h
valarray_before.h
values.h
vfio.h
vfs.h

```



## Lasso


```Lasso
// care only about visible files and filter out any directories
define dir -> eachVisibleFilePath() => {
	return with name in self -> eachEntry where #name -> second != io_dir_dt_dir where not(#name -> first  ->  beginswith('.')) select .makeFullPath(#name -> first)
}

// care only about visible directories and filter out any files
define dir -> eachVisibleDir() => {
	return with name in self -> eachEntry where #name -> second == io_dir_dt_dir where not(#name -> first -> beginswith('.')) select dir(.makeFullPath(#name -> first + '/'))
}

// Recursively walk the directory tree and find all files and directories
// return only paths to files
define dir -> eachVisibleFilePathRecursive(-dirFilter = void) => {
	local(files = .eachVisibleFilePath)
	with dir in .eachVisibleDir
	where !#dirFilter || #dirFilter(#dir -> realPath)
	do {
		#files = tie(#files, #dir -> eachVisibleFilePathRecursive(-dirFilter = #dirFilter))
	}
	return #files
}

local(matchingfilenames = array)

with filepath in dir('/') -> eachVisibleFilePathRecursive
where #filepath -> endswith('.lasso')
let filename = #filepath -> split('/') -> last
do #matchingfilenames -> insert(#filename)

#matchingfilenames
```

-> array(myfile.lasso, test.lasso, rosetta.lasso)


## LiveCode


```LiveCode
function recurDir dir, ext
    set the defaultFolder to dir
    repeat for each line fi in the files
        if fileExt(fi) = ext then
            put the longfilepath of fi & cr after fileList
        end if
    end repeat
    repeat for each line di in the folders
        if di is not "." and di is not ".." then
            put recurDir((dir & slash & di), ext)  & cr after fileList
        end if
    end repeat
    filter fileList without empty
    return fileList
end recurDir

function fileExt f
    set the itemdel to "."
    return the last item of f
end fileExt
```

Example

```LiveCode
put recurDir(the home folder & slash & "music", "mp3")
```

Output

```txt
... /Users/xxx/music/albumx/trackx.mp3
/Users/xxx/music/albumx/trackx2.mp3
/Users/xxx/music/albumy/tracky.mp3 ...
```


## Lua

Lua provides functions such as os.execute([command]) and io.popen(prog [, mode]). Below an example for Windows users having io.popen at their disposal. Mind you, it may pop-up a command window.

```Lua
-- Gets the output of given program as string
-- Note that io.popen is not available on all platforms
local function getOutput(prog)
    local file = assert(io.popen(prog, "r"))
    local output = assert(file:read("*a"))
    file:close()
    return output
end

-- Iterates files in given directory
local function files(directory, recursively)
    -- Use windows" dir command
    local directory = directory:gsub("/", "\\")
    local filenames = getOutput(string.format("dir %s %s/B/A:A", directory, recursively and '/S' or ''))

    -- Function to be called in "for filename in files(directory)"
    return coroutine.wrap(function()
        for filename in filenames:gmatch("([^\r\n]+)") do
            coroutine.yield(filename)
        end
    end)
end

-- Walk "C:/Windows" looking for executables
local directory = "C:/Windows"
local pattern = ".*%.exe$" -- for finding executables
for filename in files(directory, true) do
    if filename:match(pattern) then
        print(filename)
    end
end
```



## Mathematica

The built-in function <code>FileNames</code> does exactly this:
:<code>FileNames[]</code> lists all files in the current working directory.
:<code>FileNames[form]</code> lists all files in the current working directory whose names match the string pattern form.
:<code>FileNames[{form1,form2,...}]</code> lists all files whose names match any of the form_i.
:<code>FileNames[forms,{dir1,dir2,...}]</code> lists files with names matching forms in any of the directories dir_i.
:<code>FileNames[forms,dirs,n]</code> includes files that are in subdirectories up to n levels down.
Examples (find all files in current directory, find all png files in root directory, find all files on the hard drive):

```Mathematica
FileNames["*"]
FileNames["*.png", $RootDirectory]
FileNames["*", {"*"}, Infinity]
```

the result can be printed with Print /@ FileNames[....]

=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function walk_a_directory_recursively(d, pattern)
	f = dir(fullfile(d,pattern));
	for k = 1:length(f)
		fprintf('%s\n',fullfile(d,f(k).name));
	end;

	f = dir(d);
	n = find([f.isdir]);
	for k=n(:)'
		if any(f(k).name~='.')
			walk_a_directory_recursively(fullfile(d,f(k).name), pattern);
		end;
	end;
end;
```



## MAXScript


```maxscript
fn walkDir dir pattern =
(
    dirArr = GetDirectories (dir + "\\*")

    for d in dirArr do
    (
        join dirArr (getDirectories (d + "\\*"))
    )

    append dirArr (dir + "\\") -- Need to include the original top level directory

    for f in dirArr do
    (
        print (getFiles (f + pattern))
    )
)

walkDir "C:" "*.txt"
```



## Nim


```nim
import os, re

for file in walkDirRec "/":
  if file.match re".*\.mp3":
    echo file
```



## Objeck


```objeck
use System.IO.File;

class Test {
  function : Main(args : String[]) ~ Nil {
    if(args->Size() = 2) {
      DescendDir(args[0], args[1]);
    };
  }

  function : DescendDir(path : String, pattern : String) ~ Nil {
    files := Directory->List(path);
    each(i : files) {
      file := files[i];
      if(<>file->StartsWith('.')) {
        dir_path := String->New(path);
        dir_path += '/';
        dir_path += file;

        if(Directory->Exists(dir_path)) {
          DescendDir(dir_path, pattern);
        }
        else if(File->Exists(dir_path) & dir_path->EndsWith(pattern)) {
          dir_path->PrintLine();
        };
      };
    };
  }
}
```


=={{header|Objective-C}}==

```objc
NSString *dir = NSHomeDirectory();
NSDirectoryEnumerator *de = [[NSFileManager defaultManager] enumeratorAtPath:dir];

for (NSString *file in de)
  if ([[file pathExtension] isEqualToString:@"mp3"])
    NSLog(@"%@", file);
```



## OCaml


```ocaml
#!/usr/bin/env ocaml
#load "unix.cma"
#load "str.cma"
open Unix

let walk_directory_tree dir pattern =
  let re = Str.regexp pattern in (* pre-compile the regexp *)
  let select str = Str.string_match re str 0 in
  let rec walk acc = function
  | [] -> (acc)
  | dir::tail ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs,files) f ->
             match (stat f).st_kind with
             | S_REG -> (dirs, f::files)  (* Regular file *)
             | S_DIR -> (f::dirs, files)  (* Directory *)
             | _ -> (dirs, files)
          ) ([],[]) contents
      in
      let matched = List.filter (select) files in
      walk (matched @ acc) (dirs @ tail)
  in
  walk [] [dir]
;;

let () =
  let results = walk_directory_tree "/usr/local/lib/ocaml"  ".*\\.cma" in
  List.iter print_endline results;
;;
```



## ooRexx


### version 1


```oorexx
/* REXX ---------------------------------------------------------------
* List all file names on my disk D: that contain the string TTTT
*--------------------------------------------------------------------*/
call SysFileTree "d:\*.*", "file", "FS" -- F get all Files
                                        -- S search subdirectories
Say file.0 'files on disk'
do i=1 to file.0
  If pos('TTTT',translate(file.i))>0 Then
    say file.i
  end
```

```txt
1127869 files on disk
 1/21/15  10:31p         340  A----  d:\tttt.txt
 1/21/15  10:37p           8  A----  d:\test\test2\test3\attttb.txt
 1/21/15  10:32p         340  A----  d:\_l\TtTttt.txt
```


### version 2

Get only files matching the file-spec.

```oorexx
/* REXX ---------------------------------------------------------------
* List all file names on my disk D: that contain the string TTTT
*--------------------------------------------------------------------*/
call SysFileTree "*TTTT*.*", "file", "FS" -- F get all Files
                                        -- S search subdirectories
Say file.0 'files found'
do i=1 to file.0
  If pos('TTTT',translate(file.i))>0 Then
    say file.i
  end
```

```txt
3 files found
 1/21/15  10:31p         340  A----  D:\tttt.txt
 1/21/15  10:37p           8  A----  D:\test\test2\test3\attttb.txt
 1/21/15  10:32p         340  A----  D:\_l\TtTttt.txt
```



## Oz


```oz
declare
  [Path] = {Module.link ['x-oz://system/os/Path.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  proc {WalkDirTree Root Pattern Proc}
     proc {Walk R}
        Entries = {Path.readdir R}
        Files = {Filter Entries Path.isFile}
        MatchingFiles = {Filter Files fun {$ File} {Regex.search Pattern File} \= false end}
        Subdirs = {Filter Entries Path.isDir}
     in
        {ForAll MatchingFiles Proc}
        {ForAll Subdirs Walk}
     end
  in
     {Walk Root}
  end
in
  {WalkDirTree "." ".*\\.oz$" System.showInfo}
```



## Perl


Use the <tt>File::Find</tt> module from CPAN:

```perl
use File::Find qw(find);
my $dir      = '.';
my $pattern  = 'foo';
my $callback = sub { print $File::Find::name, "\n" if /$pattern/ };
find $callback, $dir;
```


Or if you need maximum performance and are on a 'nix system, open a pipe to the GNU <tt>find</tt> program:


```perl
sub shellquote { "'".(shift =~ s/'/'\\''/gr). "'" }

sub find_files {
    my $dir = shellquote(shift);
    my $test = shellquote(shift);

    local $/ = "\0";
    open my $pipe, "find $dir -iname $test -print0 |" or die "find: $!.\n";
    while (<$pipe>) { print "$_\n"; }  # Here you could do something else with each file path, other than simply printing it.
    close $pipe;
}

find_files('.', '*.mp3');
```



## Perl 6


Using the [https://github.com/tadzik/File-Find/ File::Find] module:

```perl6
use File::Find;

.say for find dir => '.', name => /'.txt' $/;
```


Alternatively, a custom solution that provides the same interface as the built-in (non-recursive) <tt>dir</tt> function, and uses <tt>gather</tt>/<tt>take</tt> to return a lazy sequence:


```perl6
sub find-files ($dir, Mu :$test) {
    gather for dir $dir -> $path {
        if $path.basename ~~ $test { take $path }
        if $path.d                 { .take for find-files $path, :$test };
    }
}

.put for find-files '.', test => /'.txt' $/;
```


Or if you value performance over portability, here's a function that runs the GNU <tt>find</tt> program and returns a lazy sequence of the files it finds. Parameters are not subjected to shell expansion, and the null-byte (which cannot be present in file paths) is used as the path delimiter, so it should be pretty safe.


```perl6
sub find-files ($dir, :$pattern) {
    run('find', $dir, '-iname', $pattern, '-print0', :out, :nl«\0»).out.lines;
}

.say for find-files '.', pattern => '*.txt';
```



## Phix

There is a builtin routine for this, walk_dir() - if interested you can find the full implementation in builtins\file.e (an autoinclude).

```Phix
function find_pfile(string pathname, sequence dirent)
    if match("pfile.e",dirent[D_NAME]) then
--      return pathname&dirent[D_NAME]      -- to terminate scan
        ?pathname&"\\"&dirent[D_NAME]
    end if
    return 0
end function

?walk_dir("C:\\Program Files (x86)\\Phix",routine_id("find_pfile"),1)
```

Passing 1 as the third parameter makes it scan recursively.
```txt

"C:\\Program Files (x86)\\Phix\\.hg\\store\\data\\builtins\\pfile.e.i"
"C:\\Program Files (x86)\\Phix\\builtins\\pfile.e"
0

```

<small>[the final 0 is from the walk_dir() call, whereas both paths are printed from inside find_pfile()]</small>


## PHP


```PHP
function findFiles($dir = '.', $pattern = '/./'){
  $prefix = $dir . '/';
  $dir = dir($dir);
  while (false !== ($file = $dir->read())){
    if ($file === '.' || $file === '..') continue;
    $file = $prefix . $file;
    if (is_dir($file)) findFiles($file, $pattern);
    if (preg_match($pattern, $file)){
      echo $file . "\n";
    }
  }
}
findFiles('./foo', '/\.bar$/');
```

This implementation uses Perl compatible regular expressions to match the whole path of the file

===PHP BFS (Breadth First Search)===

```PHP
/*
	This script performs a BFS search with recursion protection
	it is often faster to search using this method across a
	filesystem due to a few reasons:

	* filesystem is accessed in native node order
	* a recursive function is not required allowing infinate depth
	* multiple directory handles are not required
	* the file being searched for is often not that deep in the fs

	This method also leverages PHP array hashing to speed up loop
	detection while minimizing the amount of RAM used to track the
	search history.

	-Geoffrey McRae
	Released as open license for any use.
*/
if ($_SERVER['argc'] < 3) {
	printf(
		"\n" .
		"Usage: %s (path) (search) [stop]\n" .
		"	path	the path to search\n" .
		"	search	the filename to search for\n" .
		"	stop	stop when file found, default 1\n" .
		"\n"
		, $_SERVER['argv'][0]);
	exit(1);
}

$path   = $_SERVER['argv'][1];
$search = $_SERVER['argv'][2];
if ($_SERVER['argc'] > 3)
	$stop = $_SERVER['argv'][3] == 1;
else	$stop = true;

/* get the absolute path and ensure it has a trailing slash */
$path = realpath($path);
if (substr($path, -1) !== DIRECTORY_SEPARATOR)
	$path .= DIRECTORY_SEPARATOR;

$queue = array($path => 1);
$done  = array();
$index = 0;
while(!empty($queue)) {
	/* get one element from the queue */
	foreach($queue as $path => $unused) {
		unset($queue[$path]);
		$done[$path] = null;
		break;
	}
	unset($unused);

	$dh = @opendir($path);
	if (!$dh) continue;
	while(($filename = readdir($dh)) !== false) {
		/* dont recurse back up levels */
		if ($filename == '.' || $filename == '..')
			continue;

		/* check if the filename matches the search term */
		if ($filename == $search) {
			echo "$path$filename\n";
			if ($stop)
				break 2;
		}

		/* get the full path */
		$filename = $path . $filename;

		/* resolve symlinks to their real path */
		if (is_link($filename))
			$filename = realpath($filename);

		/* queue directories for later search */
		if (is_dir($filename)) {
			/* ensure the path has a trailing slash */
			if (substr($filename, -1) !== DIRECTORY_SEPARATOR)
				$filename .= DIRECTORY_SEPARATOR;

			/* check if we have already queued this path, or have done it */
			if (array_key_exists($filename, $queue) || array_key_exists($filename, $done))
				continue;

			/* queue the file */
			$queue[$filename] = null;
		}
	}
	closedir($dh);
}
```



## PicoLisp


```PicoLisp
(let Dir "."
   (recur (Dir)
      (for F (dir Dir)
         (let Path (pack Dir "/" F)
            (cond
               ((=T (car (info Path)))             # Is a subdirectory?
                  (recurse Path) )                 # Yes: Recurse
               ((match '`(chop "s@.l") (chop F))   # Matches 's*.l'?
                  (println Path) ) ) ) ) ) )       # Yes: Print it
```

```txt
"./src64/sym.l"
"./src64/subr.l"
...
```



## Pop11

Built-in procedure <code>sys_file_match</code> searches directories or directory trees using shell-like patterns (three dots indicate search for subdirectory tree).

```pop11
lvars repp, fil;
;;; create path repeater
sys_file_match('.../*.p', '', false, 0) -> repp;
;;; iterate over paths
while (repp() ->> fil) /= termin do
     ;;; print the path
     printf(fil, '%s\n');
endwhile;
```



## PowerShell

In PowerShell the <code>Get-ChildItem</code> cmdlet allows for recursive filtering on file names with simple wildcards:

```powershell
Get-ChildItem -Recurse -Include *.mp3
```

For more complex filtering criteria the result of <code>Get-ChildItem</code> can be piped into the <code>Where-Object</code> cmdlet:

```powershell
Get-ChildItem -Recurse |
  Where-Object { $_.Name -match 'foo[0-9]' -and $_.Length -gt 5MB }
```

To perform an action on every matching file the results can be piped into the <code>ForEach-Object</code> cmdlet:

```powershell
Get-ChildItem -Recurse |
  Where-Object { $_.Name -match 'foo[0-9]' } |
  ForEach-Object { ... }
```

''Note:'' To include only ''files'' instead of directories too each of the above needs an additional<code>Where-Object</code> filter:

```powershell
| Where-Object { !$_.PSIsContainer }
```



## PureBasic


```PureBasic
Procedure.s WalkRecursive(dir,path.s,Pattern.s="\.txt$")
  Static RegularExpression
  If Not RegularExpression
    RegularExpression=CreateRegularExpression(#PB_Any,Pattern)
  EndIf

  While NextDirectoryEntry(dir)
    If DirectoryEntryType(dir)=#PB_DirectoryEntry_Directory
      If DirectoryEntryName(dir)<>"." And DirectoryEntryName(dir)<>".."
        If ExamineDirectory(dir+1,path+DirectoryEntryName(dir),"")
          WalkRecursive(dir+1,path+DirectoryEntryName(dir)+"\",Pattern)
          FinishDirectory(dir+1)
        Else
          Debug "Error in "+path+DirectoryEntryName(dir)
        EndIf
      EndIf
    Else ; e.g. #PB_DirectoryEntry_File
      If MatchRegularExpression(RegularExpression,DirectoryEntryName(dir))
        Debug DirectoryEntryName(dir)
      EndIf
    EndIf
  Wend
EndProcedure
```


```PureBasic
;- Implementation; Find all .log-files in the C:\Windows tree
ExamineDirectory(1,"C:\WINDOWS\","")
WalkRecursive(1,"C:\WINDOWS\","\.log$")
FinishDirectory(1)
```



## Python

This uses the standard [http://docs.python.org/py3k/library/os.html?highlight=os.walk#os.walk os.walk()] module function to walk a directory tree, and the [http://docs.python.org/py3k/library/fnmatch.html fnmatch] module for matching file names.

```python
import fnmatch
import os

rootPath = '/'
pattern = '*.mp3'

for root, dirs, files in os.walk(rootPath):
    for filename in fnmatch.filter(files, pattern):
        print( os.path.join(root, filename))
```

A more strictly comparable port of this 2.3+ code to earlier versions of Python would be:

```python
from fnmatch import fnmatch
import os, os.path

def print_fnmatches(pattern, dir, files):
    for filename in files:
        if fnmatch(filename, pattern):
            print os.path.join(dir, filename)

os.path.walk('/', print_fnmatches, '*.mp3')
```

The old ''os.path.walk'' function was a challenge for many to use because of the need to pass a function into the walk, and any arguments to that function through to it ... as shown.  It's sometimes useful to pass mutable objects (lists, dictionaries, or instances of user-defined classes) to the inner function ... for example, to collect all the matching files for later processing.

Of course the function being passed down through ''os.path.walk()'' can also be an instance of an object which maintains it's own data collections.  Any matching criteria can be set as attributes of that object in advance and methods of that object can be called upon for later processing as well.  That would the an object oriented approach which would obviate the need for the "arguments" to be passed through ''os.path.walk()'' at all.
(''Note:'' This uses a non-standard replacement to the '''os.path''' module)

```python
from path import path

rootPath = '/'
pattern = '*.mp3'

d = path(rootPath)
for f in d.walkfiles(pattern):
  print f
```



## R


```R
dir("/bar/foo", "mp3",recursive=T)
```



## Racket



```Racket

-> (for ([f (in-directory "/tmp")] #:when (regexp-match? "\\.rkt$" f))
     (displayln f))
... *.rkt files including in nested directories ...

```



## Rascal


```rascal
//usage example: To list just Rascal source files,    Walk(|home:///workspace/|, ".rsc");
module Walk
import String;
import IO;
public void Walk(loc a, str pattern){
	for (entry <- listEntries(a))
		if (endsWith(entry, pattern))
		 	println(entry);
		elseif (isDirectory(a+entry))
			Walk(a+entry, pattern);
}
```



## REALbasic


```vb
Sub printFiles(parentDir As FolderItem, pattern As String)
  For i As Integer = 1 To parentDir.Count
    If parentDir.Item(i).Directory Then
      printFiles(parentDir.Item(i), pattern)
    Else
      Dim rg as New RegEx
      Dim myMatch as RegExMatch
      rg.SearchPattern = pattern
      myMatch = rg.search(parentDir.Item(i).Name)
      If myMatch <> Nil Then Print(parentDir.Item(i).AbsolutePath)
    End If
  Next
End Sub
```

Accepts a FolderItem object and a Regex pattern as a string:

```vb

  Dim f As FolderItem = GetFolderItem("C:\Windows\system32")
  Dim pattern As String = "((?:[a-z][a-z]+))(\.)(dll)"  //all file names ending in .dll
  printFiles(f, pattern)
```



## REXX


### version 1

The following program was tested in a DOS window under Windows/XP and should work for all Microsoft Windows.

```rexx
/*REXX program shows all files in a  directory tree  that match a given search criteria.*/
parse arg xdir;  if xdir=''  then xdir='\'       /*Any DIR specified?  Then use default.*/
@.=0                                             /*default result in case ADDRESS fails.*/
dirCmd= 'DIR /b /s'                              /*the DOS command to do heavy lifting. */
trace off                                        /*suppress REXX error message for fails*/
address system  dirCmd xdir  with output stem @. /*issue the DOS DIR command with option*/
if rc\==0  then do                               /*did the DOS DIR command get an error?*/
                say '***error!*** from DIR' xDIR /*error message that shows "que pasa". */
                say 'return code='  rc           /*show the  return code  from  DOS DIR.*/
                exit rc                          /*exit with    "     "     "    "   "  */
                end                              /* [↑]  bad ADDRESS cmd  (from DOS DIR)*/
#=@.rc                                           /*the number of  @.  entries generated.*/
if #==0  then #='   no   '                       /*use a better word choice for 0 (zero)*/
say center('directory '      xdir      " has "      #       ' matching entries.', 79, "─")

                do j=1  for #;       say @.j     /*show all the files that met criteria.*/
                end   /*j*/
exit @.0+rc                                      /*stick a fork in it,  we're all done. */
```

'''output'''   when the following was used:   <tt> I:\firefox*.exe </tt>

```txt

─────────────directory  I:\firefox*.exe  has  6  matching entries.─────────────
I:\FIREFOX\firefox.exe
I:\FIREFOX\INSTALL\Firefox Setup 1.5.0.1.exe
I:\FIREFOX\INSTALL\Firefox Setup 2.0.0.4.exe
I:\FIREFOX\INSTALL\Firefox Setup 3.0.4.exe
I:\FIREFOX\INSTALL\Firefox Setup 3.6 Beta 5.exe
I:\FIREFOX\INSTALL\Firefox Setup 4.0 Beta 11.exe

```



### version 2

Works on Windows with ooRexx and Regina (not much REXX code in it)

```rexx
'dir /s /b "%windir%\System32\*.exe"'
```



## Ring


```ring

see "Testing DIR() " + nl
mylist = dir("C:\Ring")
for x in mylist
    if x[2]
       see "Directory : " + x[1] + nl
    else
       see "File : " + x[1] + nl
    ok
next
see "Files count : " + len(mylist)

```

Output:

```txt

Testing DIR()
Directory : bert
Directory : bin
Directory : calmosoft
Directory : doc
Directory : FlappyBird
Directory : gameengine
Directory : html
Directory : images
File : License.txt
File : music1.wav
File : ReadMe.txt
Directory : ring-master
Directory : samples
Directory : StarsFighter
File : start.bat
Directory : stdlib
Directory : SuperMan2016
File : unixdict.txt
Directory : weblib
Files count : 19

```



## Ruby


Using the Find core Module:


```ruby
require 'find'

Find.find('/your/path') do |f|
   # print file and path to screen if filename ends in ".mp3"
   puts f if f.match(/\.mp3\Z/)
end
```


A little less verbose example using a shortcut for the glob method of Dir:


```ruby
puts Dir['**/*.mp3']
```



## Rust


Using std::fs::walk_dir (unstable as of Rust 1.1) with imperative for-loop:


```rust
#![feature(fs_walk)]

use std::fs;
use std::path::Path;

fn main() {
    for f in fs::walk_dir(&Path::new("/home/pavel/Music")).unwrap() {
        let p = f.unwrap().path();
        if p.extension().unwrap_or("".as_ref()) == "mp3" {
            println!("{:?}", p);
        }
    }
}
```



## Scala

This is not implemented in the Scala library. Here is a simple solution, building on [[Java]] class ''<code>java.io.File</code>'':


```scala
import java.io.File

object `package` {
  def walkTree(file: File): Iterable[File] = {
    val children = new Iterable[File] {
      def iterator = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
    }
    Seq(file) ++: children.flatMap(walkTree(_))
  }
}

object Test extends App {
  val dir = new File("/home/user")
  for(f <- walkTree(dir)) println(f)
  for(f <- walkTree(dir) if f.getName.endsWith(".mp3")) println(f)
}
```



## Scheme

Varies slightly depending on the implementation of scheme.
```scheme
(use posix)
(use files)
(use srfi-13)

(define (walk FN PATH)
  (for-each (lambda (ENTRY)
    (cond ((not (null? ENTRY))

	   (let ((MYPATH (make-pathname PATH ENTRY)))

	     (cond ((directory-exists? MYPATH)
		    (walk FN MYPATH) ))

	     (FN MYPATH) )))) (directory PATH #t) ))

(walk (lambda (X) (cond ((string-suffix? ".scm" X) (display X)(newline) ))) "/home/user/")
```

See also: '''(find-files ...)''' function in the '''posix''' module.
```scheme
(use file.util)
(use srfi-13)

(define (walk FN PATH)
  (for-each (lambda (ENTRY)
    (cond ((not (null? ENTRY))
	   (let ((MYPATH ENTRY))

	     (cond ((file-is-directory? MYPATH)
		    (walk FN MYPATH) ))

	     (FN MYPATH) )))) (directory-list PATH :add-path? #t :children? #t ) ))

(walk (lambda (X) (cond ((string-suffix? ".scm" X) (display X)(newline) ))) "/home/user/")
```

See also: '''(find-file-in-paths ...)''' function in the '''file.util''' module.
```scheme
#lang scheme

(require srfi/13)

(define (walk FN PATH)
  (for-each (lambda (ENTRY)
    (cond ((not (null? ENTRY))

	   (let ((MYPATH (build-path PATH ENTRY)))

	     (cond ((directory-exists? MYPATH)
		    (walk FN MYPATH) ))

	     (FN MYPATH) )))) (directory-list PATH)))

(walk (lambda (X) (cond ((string-suffix? ".scm" (path->string X)) (display X)(newline) ))) "/home/user/")
```

See also: '''(find-files ...)''' function in the '''file''' module.
```txt

/home/user/one.scm
/home/user/lang/two.scm
[...]

```



## Seed7

Seed7 has a [http://seed7.sourceforge.net/manual/os.htm#Standard_path_representation standard path representation],
which is independent of the operating system. The function [http://seed7.sourceforge.net/manual/os.htm#readDir readDir]
reads the contents of a directory as array of strings. The files . and .. are left out, so it is not necessary to ignore them.
The function [http://seed7.sourceforge.net/manual/os.htm#fileType fileType] is used to determine, if a file is a directory.
The example below follows symbolic links. To ignore symbolic links use
[http://seed7.sourceforge.net/libraries/osfiles.htm#fileTypeSL%28in_string%29 fileTypeSL] instead of
[http://seed7.sourceforge.net/libraries/osfiles.htm#fileType%28in_string%29 fileType].


```seed7
$ include "seed7_05.s7i";
  include "osfiles.s7i";

const proc: walkDir (in string: dirName, in string: extension) is func
  local
    var string: fileName is "";
    var string: path is "";
  begin
    for fileName range readDir(dirName) do
      path := dirName & "/" & fileName;
      if endsWith(path, extension) then
        writeln(path);
      end if;
      if fileType(path) = FILE_DIR then
        walkDir(path, extension);
      end if;
    end for;
  end func;

const proc: main is func
  begin
    walkDir(".", ".sd7");
  end func;
```



## Sidef


```ruby
func traverse(Block callback, Dir dir) {
    dir.open(\var dir_h) || return nil
 
    dir_h.entries.each { |entry|
        if (entry.is_a(Dir)) {
            traverse(callback, entry)
        } else {
            callback(entry)
        }
    }
}
 
var dir = Dir.cwd
var pattern = /foo/   # display files that contain 'foo'
 
traverse(
    { |file|
        if (file.basename ~~ pattern) {
            say file
        }
    } => dir
)
```



## Smalltalk

```smalltalk
Directory extend [
  wholeContent: aPattern do: twoBlock [
    self wholeContent: aPattern withLevel: 0 do: twoBlock.
  ]
  wholeContent: aPattern withLevel: l do: twoBlock [
    |cont|
    cont := (self contents) asSortedCollection.
    cont remove: '.'; remove: '..'.
    cont
    do: [ :n | |fn ps|
      ps := (Directory pathSeparator) asString.
      fn := (self name), ps, n.
      ((File name: fn) isDirectory)
      ifTrue: [
        twoBlock value: (n, ps) value: l.
	(Directory name: fn) wholeContent: aPattern withLevel: (l+1) do: twoBlock.
      ]
      ifFalse: [
        ( n =~ aPattern )
        ifMatched: [ :m |
          twoBlock value: n value: l
        ]
      ]
    ]
  ]
].
```



```smalltalk
|d|
d := Directory name: '.'.
d wholeContent: '\.st$' do: [ :f :l |
   0 to: l do: [ :i | (Character tab) display ].
   f displayNl
].
```



## Swift

```swift
import Foundation

let fileSystem = FileManager.default
let rootPath = "/"

// Enumerate the directory tree (which likely recurses internally)...

if let fsTree = fileSystem.enumerator(atPath: rootPath) {

    while let fsNodeName = fsTree.nextObject() as? NSString {

        let fullPath = "\(rootPath)/\(fsNodeName)"

        var isDir: ObjCBool = false
        fileSystem.fileExists(atPath: fullPath, isDirectory: &isDir)

        if !isDir.boolValue && fsNodeName.pathExtension == "txt" {
            print(fsNodeName)
        }
    }
}
```



## Tcl

```tcl

package require fileutil
proc walkin {path cmd} {
    set normalized [::fileutil::fullnormalize $path]
    set myname [lindex [info level 0] 0]
    set children [glob -nocomplain -directory $path -types hidden *]
    lappend children {*}[glob -nocomplain -directory $path *]
    foreach child $children[set children {}] {
        if {[file tail $child] in {. ..}} {
            continue
        }
        if {[file isdirectory $child]} {
            if {[file type $child] eq "link"} {
                set normalizedchild [fileutil::fullnormalize $child]
                if {[string first $normalized/ $normalizedchild] == 0} {
                    #symlink to a directory in $path.  Avoid cyclic traversal.
                    #Don't descend.
                } else {
                    $myname $child $cmd
                }
            }
        }
        {*}$cmd $child
    }
}

walkin /home/usr {apply {fname {
    set tail [file tail $fname]
    if {[string match *.mp3 $tail]} {
        puts $fname
    }
}}}

```



## TXR

There is more than one way to do this in TXR. A recursive walk could be coded using <code>open-directory</code> and <code>getline</code>. Or FFI could be used to gain access to some platform-specific functions like Microsoft's <code>FindFirstFile</code> and so forth.

TXR wraps and exposes the POSIX <code>nftw</code> function, which is demonstrated here. This function encapsulates a tree walk, and uses callbacks to inform the program of visited filesystem tree nodes, and of error situations. We can use a <code>lambda</code> for the code walk, or wrap the invocation of <code>ftw</code> with a macro which hides the <code>lambda</code> syntax.


Here we use the <code>build</code> macro for procedural list building to gather all of the found paths into a list, which is implicitly returned. The callback is an explicit <code>lambda</code>:


```txrlisp
(build (ftw "." (lambda (path type stat level base)
                  (if (ends-with ".tl" path)
                    (add path)))))
```


```txrlisp
("./tests/016/arith.tl" "./tests/014/dgram-stream.tl" "./tests/014/socket-basic.tl"
 "./tests/sock-common.tl" "./tests/012/ifa.tl" "./tests/012/except.tl"
 "./tests/012/fini.tl" "./tests/012/oop.tl" "./tests/012/circ.tl"
 "./tests/012/cont.tl" "./tests/012/aseq.tl" "./tests/012/quasi.tl"
 "./tests/012/struct.tl" "./tests/012/man-or-boy.tl" "./tests/017/glob-carray.tl"
 "./tests/017/glob-zarray.tl" "./tests/017/realpath.tl" "./tests/017/qsort.tl"
 "./tests/015/split.tl" "./tests/013/maze.tl" "./tests/common.tl"
 "./tests/011/special-1.tl" "./share/txr/stdlib/ifa.tl" "./share/txr/stdlib/with-stream.tl"
 "./share/txr/stdlib/pmac.tl" "./share/txr/stdlib/except.tl" "./share/txr/stdlib/awk.tl"
 "./share/txr/stdlib/package.tl" "./share/txr/stdlib/place.tl"
 "./share/txr/stdlib/trace.tl" "./share/txr/stdlib/type.tl" "./share/txr/stdlib/keyparams.tl"
 "./share/txr/stdlib/ffi.tl" "./share/txr/stdlib/ver.tl" "./share/txr/stdlib/build.tl"
 "./share/txr/stdlib/cadr.tl" "./share/txr/stdlib/hash.tl" "./share/txr/stdlib/error.tl"
 "./share/txr/stdlib/txr-case.tl" "./share/txr/stdlib/tagbody.tl"
 "./share/txr/stdlib/getopts.tl" "./share/txr/stdlib/socket.tl"
 "./share/txr/stdlib/struct.tl" "./share/txr/stdlib/getput.tl"
 "./share/txr/stdlib/path-test.tl" "./share/txr/stdlib/with-resources.tl"
 "./share/txr/stdlib/yield.tl" "./share/txr/stdlib/conv.tl" "./share/txr/stdlib/termios.tl")
```


For a regex pattern we can replace <code>(endswith ".tl" path)</code> with something like <code>(m$ path #/\.tl/)</code>.

TXR also provides the <code>fnmatch</code> function which can be used to match using a file globbing pattern.


```txt
1< (fnmatch "*.tl" "foo.tl")
t
2>< (fnmatch "*.tl" "foo.c")
nil
```


The <code>type</code>, <code>stat</code>, <code>level</code> and <code>base</code> callback arguments we are ignoring closely follow those of the POSIX C <code>nftw</code> function. <code>type</code> is a type code which indicates the kind of item visited: file, directory; <code>stat</code> is a Lisp version of <code>struct stat</code>, providing various information about the filesystem object: permissions, timestamps, inode number, etc.

A nice approach would be to capture a continuation in the callback, and then obtain the walk elements lazily; alas, capturing a continuation from a C library function's callback is not permitted, because the capture would span foreign stack frames.


## UNIX Shell

The "find" command gives a one-line solution for simple patterns:

```bash
find . -name '*.txt' -type f
```

"find" can also be used to find files matching more complex patterns as illustrated in the section on [[#UnixPipes|Unix Pipes]] below.

Using "bash" version 4 or later, you can use "globstar" or "dotglob", depending on whether you want hidden directories to be searched:

```bash
#! /bin/bash
# Warning: globstar excludes hidden directories.
# Turn on recursive globbing (in this script) or exit if the option is not supported:
shopt -s globstar || exit

for f in **
do
  if [[ "$f" =~ \.txt$ ]] ; then
    echo "$f"
  fi
done
```

Here is a solution that does not use "find".

```bash
#! /bin/bash

indent_print()
{
    for((i=0; i < $1; i++)); do
	echo -ne "\t"
    done
    echo "$2"
}

walk_tree()
{
    local oldifs bn lev pr pmat
    if [[ $# -lt 3 ]]; then
	if [[ $# -lt 2 ]]; then
	    pmat=".*"
	else
	    pmat="$2"
	fi
	walk_tree "$1" "$pmat" 0
	return
    fi
    lev=$3
    [ -d "$1" ] || return
    oldifs=$IFS
    IFS="
"
    for el in $1/*; do
	bn=$(basename "$el")
	if [[ -d "$el" ]]; then
	    indent_print $lev "$bn/"
	    pr=$( walk_tree "$el" "$2" $(( lev + 1)) )
	    echo "$pr"
	else
	    if [[ "$bn" =~ $2 ]]; then
		indent_print $lev "$bn"
	    fi
	fi
    done
    IFS=$oldifs
}

walk_tree "$1" "\.sh$"
```

A simplified version that gives the same output:

```bash
#! /usr/bin/env bash

walk_tree() {
	ls "$1" | while IFS= read i; do
		if [ -d "$1/$i" ]; then
			echo "$i/"
			walk_tree "$1/$i" "$2" | sed -r 's/^/\t/'
		else
			echo "$i" | grep -E "$2"
		fi
	done
}

walk_tree "$1" "\.sh$"
```



## UnixPipes

As illustrated [[#UNIX Shell|above]], the "find" command can be used with the -name option to match simple patterns.  To find files matching more complex patterns, the results of "find" can be piped, e.g.

```bash
find . -type f | egrep '\.txt$|\.TXT$'
```

One way to run a command against each file that is found is to use "xargs", but if there is any possibility that a filename contains a space or tab character, then the following model should be used:

```bash
 find . -type f -name "*.txt" -print0 | xargs -0 fgrep sometext
```



## Visual Basic .NET

This uses the OS pattern matching

```vbnet
Sub walkTree(ByVal directory As IO.DirectoryInfo, ByVal pattern As String)
    For Each file In directory.GetFiles(pattern)
        Console.WriteLine(file.FullName)
    Next
    For Each subDir In directory.GetDirectories
        walkTree(subDir, pattern)
    Next
End Sub
```



## zkl


```zkl
d:=File.globular("..","s*.zkl")
```

Lots of options, here I'm using the defaults: recurse, just file matches (not directory names) and return a bit bucket of ASCIIZ strings.
```txt

d.pump(Console.println)
../Tmp/sieve.zkl
../Tmp/sock2.zkl
../Tmp/strands.zkl
../Tmp/sd.zkl
../Src/startup.zkl
../Src/ZenKinetic/sieve.zkl
../Tests/subscript.zkl
../Tests/Object/socket.zkl
../Tests/Object/string.zkl

```

globular will write to a object that has a write method or just call a method or function, which is nice for sending data to other threads (eg multi-threaded find/grep). To do the above example in one shot (without saving the results):

```zkl
File.globular("..","s*.zkl",True,0,Console.println)
```



## Zsh

Zsh has recursive globbing. The GLOB_DOTS option allows files beginning with a period to be matched.

```zsh
setopt GLOB_DOTS
print -l -- **/*.txt
```

GLOB_DOTS can be set temporarily with the 'D' modifier.

```zsh
print -l -- **/*.txt(D)
```


{{omit from|Befunge}} <!-- No filesystem support -->
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have a filesystem, just namespaced variables, which can't be listed from a program. -->
