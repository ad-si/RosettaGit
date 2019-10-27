+++
title = "Talk:Run as a daemon or service"
description = ""
date = 2013-03-29T12:49:21Z
aliases = []
[extra]
id = 10878
[taxonomies]
categories = []
tags = []
+++

== Bad definition? ==

It's probably better to describe a daemon as a process that runs in the background and independent of a user's login session, if anything. 
: True, I'll try to integrate that into the description.--[[User:EMBee|eMBee]] 08:56, 17 November 2011 (UTC)
"Not connected to a terminal" isn't quite it, a daemon could print to a tty all day if it wants to.
: Yes, but only if it opens the tty manually. It should otherwise be able to run without a tty being present.--[[User:EMBee|eMBee]] 08:56, 17 November 2011 (UTC)
:: Not really.  
```c>#include <stdio.h

#include <stdlib.h>

int main(void)
{
        FILE *fp = fopen("/tmp/outlog", "w");
        while (1) {
                printf("yes\n");
                fprintf(fp, "out\n");
                fflush(fp);
                sleep(1);
        }
}
```

:: Run it as <code>./a.out & disown</code> in bash, and it will keep printing to the terminal.  It qualifies as a daemon in most senses because it will keep going (verify by the outlog file) even if you log out and kill the terminal.  It ''can'' run without a tty, doesn't mean "without a tty" is a necessary condition. --[[User:Ledrug|Ledrug]] 09:56, 17 November 2011 (UTC)
::: fair enough. however i would not consider that a clean way to write a daemon. the goal here is to write the daemon in such a way that the administrator does not have to jump through hoops to make sure that the process doesn't die if the terminal goes away and to capture all output from the daemon.--[[User:EMBee|eMBee]] 13:14, 17 November 2011 (UTC)
Also, why do you want a daemon process to both detach from stdout ''and'' print to stdout?  Wouldn't it be saner to have it directly write to a file to begin with? --[[User:Ledrug|Ledrug]] 08:23, 17 November 2011 (UTC)
: I want it to detach from the stdout that is connected to the users terminal, and write to a file as if it were stdout. Redirecting stdout is the sanest way to achieve that because the internals of the program and the libraries used can continue to write to what looks like stdout. If I open a file in the daemon itself and use that, I have to go out of my way to make sure that everywhere I have output the file is used. I have to make that file globally accessible or pass it around as a variable. There may be languages/situations where this is the better option, but I would not say that it is always better or saner.--[[User:EMBee|eMBee]] 08:56, 17 November 2011 (UTC)
:: This strikes me as bad modularity.  If you want the program detached from the shell's stdio context you can and should do that in the shell.  You should not replicate that shell-specific code in every program. 
::: i do not understand this point. every daemon would need its own shellscript because they all have different files their output needs to go to. there is nothing that shellscripts would avoid from being replicated.
::: also, depending on the language, the code to detach the daemon may be no more complex than the shellscript.--[[User:EMBee|eMBee]] 15:15, 17 November 2011 (UTC)
:: It's an unnecessary complication for programs running from inittab, for example.  For example, from bourne shell:
::
```bash
(myprog </dev/null >/dev/null 2>&1 &)
```

::This detaches <code>myprog</code> from the shell (its parent process will be init, and it obviously will not be using the shell's terminal).  And, of course, if you want to reconfigure it to log to a file (or to another process) from stdout, you could do that instead.
::But if you are really serious about running myprog as a daemon, you should probably run it from inittab (or xinittab or whatever ...), probably wrapped in <code>su</code> and so it runs as a relevant user. --[[User:Rdm|Rdm]] 14:42, 17 November 2011 (UTC)
::: an unnecesary complication is having to write a shell script in order to run a daemon. X and getty are run from inittab without a shell and we can probably find other examples, of course we can find counter examples too, but i don't see a reason prefer using a shell wrapper.
::: also there are only few developers writing the daemon, but many more sysadmins running it. so any complication that is taken away from the sysadmin and moved to the developers is a win in the long run. 
::: but the main point really is to show of the capabilities of languages here. using a shell script would defeat that purpose and would not fit on rosettacode. it also would not be portable.--[[User:EMBee|eMBee]] 15:15, 17 November 2011 (UTC)
:::: Huh?
:::: First: daemons should be run from inittab or the rc directories which are run by init.  If you don't do that, you lose your daemon on every reboot.
::::: yes--[[User:EMBee|eMBee]] 18:09, 17 November 2011 (UTC)
:::: Second: you could easily abstract the shell script:
:::: 
```bash
$ cat >/usr/local/bin/detach_from_shell; chmod +x /usr/local/bin/detach_from_shell
#!/bin/sh
if [ -n "$1" ]; then (
   "$@" </dev/null >/dev/null 2>&1&
) else 
   >&2 echo 'Usage:'
   >&2 echo 'detach_from_shell program [arguments]'
   exit 1
fi
```

:::: Now you have a shell command to run a program "as a daemon" without actually installing it on the system so that it runs reliably (e.g. so it comes back up after a reboot...)
::::: well, yes, except that this is very rare. to many services need special handling, extra arguments. some don't like their stdout redirected to /dev/null because they actually write something there, to the extent that it is rather hard to have one shell script to cover all. there is a reason why the sysv init has one shell script for each service.--[[User:EMBee|eMBee]] 18:09, 17 November 2011 (UTC)
:::: Meanwhile, if you are not running the program from the shell, there is no reason to detach it from the shell context. --[[User:Rdm|Rdm]] 17:50, 17 November 2011 (UTC)
::::: true. with a modern init system this is not needed, but it is nice to have the option and none of the arguments given here show that being able to detach from the terminal is inherently bad, nor has anyone yet explained that we shouldn't document capabilities that do exist. however all comments so far center on a solution which is outside of the scope of rosettacode because it has nothing to do with language comparison if we use a shellscript to solve the problem for any language. the shell solution will have its place in the [[UNIX Shell]] section.--[[User:EMBee|eMBee]] 18:09, 17 November 2011 (UTC)

== A different angle ==

i'd like to restart the discussion from a different angle. instead of discussing a solution which is beyond the scope of rosettacode, i'd like to ask this direct question:

* is this task suitable for rosettacode?

if the discussion until now is any indication then the answer is ''no''.

however i'd disagree with that. 
* the capability to disconnect from a tty and to redirect or close stdout,stderr,stdin from within a process does exist in more than one language so it is worth demonstrating it.
* this method of writing daemons has been used traditionally for a long time. a search for "how to write a unix daemon" has most links on the first result pages point to resources that explain just this method and do not talk about shell scripts. and it not only works on unix. (DOS TSR comes to mind)
* using a shell script is a very unix centric solution whereas an in-language solution is likely more portable.
--[[User:EMBee|eMBee]] 18:23, 17 November 2011 (UTC)
: I did not see anything in this task which was relevant to DOS TSRs.
:: see [[wp:Daemon_(computing)#Implementation_in_MS-DOS_and_Microsoft_Windows|Wikipedia]]: ''In the Microsoft DOS environment, daemon-like programs were implemented as Terminate and Stay Resident (TSR) software.''--[[User:EMBee|eMBee]] 09:19, 18 November 2011 (UTC)
::: Yes... ok, TSRs get mentioned.  But to set up a DOS TSR you need to arrange for your interrupt handler to remain after the program exits, and of course you also need to set things up so that when interrupts happen the handler gets the interrupts.  Then you need to exit the program.  But this task is not about setting up interrupt handlers under DOS.  Most of the languages here will not even run under DOS.  So, anyways, this task is not about setting up a DOS TSR, and would not be useful for setting up a DOS TSR.  --[[User:Rdm|Rdm]] 15:37, 18 November 2011 (UTC)
: As far as I know, this approach (redefining stdout) is only relevant for "daemons" when you are running the program under Unix. --[[User:Rdm|Rdm]] 18:36, 17 November 2011 (UTC)
:: from a users perspective the process ends, but it is actually still there. the fact that DOS TSR requires an interrupt for the program to wake up has more to do with the lack of multitasking than being something different alltogether.--[[User:EMBee|eMBee]] 09:10, 18 November 2011 (UTC)
: Current problems with the task:
: # It's difficult to say what constitutes a daemon.  It should run in background in some sense, and preferrably do something useful; other than that anything goes.  It may or may not hold on to a tty (getty vs sendmail); it may or may not be system wide (gnome launches a million per-user background processes upon user session start); it may or may not interact with direct user input (gpm, XIM daemon, etc).  You need to narrow it down, or change the task to just focus on detaching from parent process.
:: hmm, all these should be fine. the daemon just shouldn't interact with the shell that it was started from.--[[User:EMBee|eMBee]] 09:10, 18 November 2011 (UTC)
: # Backgrounding a process has more to do with OS than programming language.
:: but programming languages may well include support for this for different OSes so that the programmer does not have to deal with the details. or they may not. we might find out in the course of this task.--[[User:EMBee|eMBee]] 09:10, 18 November 2011 (UTC)
:  On unix you may let init adopt the process, on Windows you need to have user login script to start the process or use regsvc or svchost.  It's also not clear if any of the systray icons count as a daemon.
:: they probably would if the program disconnects from the command shell it was started from. but i don't know how that actually works on windows. i'll probably find out at some point--[[User:EMBee|eMBee]] 09:10, 18 November 2011 (UTC)
: # Redirecting stdio is useful to have the daemon not die unexpectly on hangup or something, but it's not a defining property. 
:: correct. it is a requirement specific to this task.--[[User:EMBee|eMBee]] 09:10, 18 November 2011 (UTC)
:Also your requirement of redirecting is strange: the program is supposed to write to stdout; the program is not supposed to ''really'' write to stdout; the program is not supposed to worry about where the output goes; the program is not supposed to be invoked by shell redirection, so it must know where the output goes.  Make up your mind, maybe? --[[User:Ledrug|Ledrug]] 00:10, 18 November 2011 (UTC)
:: ok, this needs clearing up: the daemon should not write to the stdout that is connected to the terminal it was started from. it may write to stdout if that has been redirected to a file, by means that are either within the daemon itself or in a separate helper program written in the same language.
:: the part with not caring was supposed to refer to functions or libraries used within the daemon which constitute the useful part, not the daemonizing part. that is, it should be possible to take an existing program, and turn it into a daemon without having to replace all references of say <code>printf(...)</code> with <code>fprintf(somefile,...)</code>--[[User:EMBee|eMBee]] 09:10, 18 November 2011 (UTC)
::: And, of course, the shell script approach supports all of this.  Redirect to /dev/null means those printf references do not have to be changed and, if stdout is redirected within the program, their output will be redirected appropriately.  --[[User:Rdm|Rdm]] 18:56, 18 November 2011 (UTC)

== Strange stdout rule ==

For some strange reason, the current draft task wants to use "stdout which should be redirected to a file". This redirection of stdout has no purpose to the daemon, and is not better than simply opening a file. To redirect stdout, my C code calls dup2().


```c
fd = open(argv[1], O_WRONLY | O_APPEND | O_CREAT, 0666);
dup2(fd, STDOUT_FILENO);
fputs("string\n", stdout);
```


But if I forgot stdout, I would simplify the code by removing open() and dup2() and using a regular fopen().


```c
file = fopen(argv[1], "a");
fputs("string\n", file);
```


I suggest to remove the stdout rule from the draft task, and to permit each solution to write to a file without regard to whether it redirects stdout. --[[User:Kernigh|Kernigh]] 00:55, 19 November 2011 (UTC)
: you are permitted to write to any file that you want, but in addition to that i'd like to know how you can do the above. this could be a seperate task, but i find tasks more interesting if they combine a few simple requirements to form a program than just questions like ''"how do you redirect stdout?"''--[[User:EMBee|eMBee]] 02:59, 19 November 2011 (UTC)

== Doh, use screen! ==

You can run processes under screen so that they always have a tty. And screen can be detached from your real tty, then re-attached later.

Screen makes an obsolete practice of in-the-app daemonization.[[Special:Contributions/24.85.131.247|24.85.131.247]] 03:34, 15 January 2012 (UTC)
: i have found screen seems to be popular among the lisp community probably in order to keep access to the repl, but i don't like it. the thing that screen does not do is restart your daemon if it disappears.--[[User:EMBee|eMBee]] 06:06, 15 January 2012 (UTC)
:: <code>while true ; yourdaemon ; done</code> [[Special:Contributions/24.85.131.247|24.85.131.247]] 03:18, 17 January 2012 (UTC)
::: sure, anything can be done by hand. but the job of a sysadmin is to automate things. if i am going to write a script, i might as well write a real init script. but then i don't need screen anymore. 
:::: An init script doesn't provide the functionality of screen. A little while loop to run a program over again is hardly a script.[[Special:Contributions/24.85.131.247|24.85.131.247]] 04:48, 17 January 2012 (UTC)
::: doing things by hand is like starting a car with empty batteries. you push it until the motor can be started. this task is about including the batteries. you start the program and it does everything by itself. no extra scripting required.--[[User:EMBee|eMBee]] 03:38, 17 January 2012 (UTC)
:::: I'm afraid you and the unsigned guy are above grasping at straws here to make a point. Pardon me, I'm only describing the simplest possible way of doing it.[[Special:Contributions/24.85.131.247|24.85.131.247]] 04:48, 17 January 2012 (UTC)
::::: the simplest way for whom? the developer? sure, because the developer then doesn't need to bother with running the daemon. for the user? well, anything extra the user needs to do is a complication. regardless how small. depending on screen certainly is one. as a developer i can't be sure that screen is available on the target machine, even if i provide all the scripts for it. so why add the extra dependency when i can write the daemon in such a way that it does not depend on additional tools just to run it as a daemon.--[[User:EMBee|eMBee]] 06:29, 17 January 2012 (UTC)
:::: A half-liner while loop to restart a program is hardly comparable to pushing a car, and such a little script can in fact be part of the application (an included battery). (Never seen Unix programs that are actually launched by shell scripts that are installed by the application installer?)[[Special:Contributions/24.85.131.247|24.85.131.247]] 04:48, 17 January 2012 (UTC)
::::: For the record (and off topic): there was no "unsigned guy" here except for anon IPs.  It's probably unfair to break up someone else's comment in halves and then blame the first half for lacking a signature. --[[User:Ledrug|Ledrug]] 05:20, 17 January 2012 (UTC)
:::::: actually, it makes it look like there is another person arguing my point. so i think that works in my favour :-) --[[User:EMBee|eMBee]] 06:29, 17 January 2012 (UTC)
: personally i use screen actively, and if i start a daemon inside screen i exactly do not want it to clutter up my screen terminals, but get out of the way.--[[User:EMBee|eMBee]] 06:06, 15 January 2012 (UTC)
:: This is not about cluttering your interactive screen sessions with daemons, but screen as a dedicated tool for having daemons running. Like under a different user ID from your account, etc! To interact with the daemons, you  might "su" to a different account and attach a different screen session.  Such screen sessions can be arranged to start at boot time with daemons in them. [[Special:Contributions/24.85.131.247|24.85.131.247]] 04:51, 17 January 2012 (UTC)
::: two screens is also clutter. if i want to run the daemon as a user that is already running screen, i get an additional screen session which adds an extra step every time i want to connect to the interactive session. and if i want to run multiple daemons do i run a separate screen session for each? or all in one session with multiple terminals? what if screen gets stuck and you can no longer connect to it to run additional daemons? yes, i have seen that happen. the only way out was to kill screen which would bring down all the daemons inside.
:::: I haven't used screen in many years, yet I certainly do remember bugs like that. But, surprisingly, it seems to be quite actively maintained.There is quite a bit of recent activity in the GIT repo: http://git.savannah.gnu.org/cgit/screen.git   What version are you using? [[Special:Contributions/24.85.131.247|24.85.131.247]] 07:31, 17 January 2012 (UTC)
:::: Then again, there are a LOT of un-investigated bugs in the database also (including one generic one about the hangs and one about attaching from another user and tty permissiosn). The most recent bug was reported a few days ago: someone ran gcc -Wall and found a few issues. That's pretty terrible for an old project.[[Special:Contributions/24.85.131.247|24.85.131.247]] 08:02, 17 January 2012 (UTC)
:::if you "su" to a user other than root, then you keep using your existing terminal and screen can't connect because the new user does not have permission to access the terminal:
 $ screen
 Cannot open your terminal '/dev/pts/1' - please check.
::: so multiple daemons in one screen session is not practical either. there are ways around it (eg. by running <code>script</code>) but that's just another complication. 
::: i am afraid we just have to agree to disagree. in my view screen is for interactive sessions. a daemon is not interactive, and there are better tools out there to run programs in the background than screen. either way, this task is not about any of those existing tools but about the capability of a language to build such a tool--[[User:EMBee|eMBee]] 06:29, 17 January 2012 (UTC)

: From my perspective, the problem with screen is that it is a manual solution at all. Daemons should run without having someone start them up by hand; after all, if it is a true daemon then it will have been set to be started automatically upon reboot of the machine (or on demand by some automated code that is itself auto-started). While I suppose I could automate screen's startup with the use of expect, the resulting hacked together lash up(/ball of mud/screaming horror from the depths) is enough to give me shivers just thinking about it. Screen is ''not'' a solution to this problem. –[[User:Dkf|Donal Fellows]] 09:43, 17 January 2012 (UTC)
:: screen has arguments to start a detached session, and run a command in that session. <code>screen -dmS <session_name> /path/to/your/server</code>. [[Special:Contributions/24.85.131.247|24.85.131.247]] 04:24, 18 January 2012 (UTC)


### Simplified description


I propose we just simplify the task description as follows:

A daemon is a service that runs in the background independent of a users login session. The task is to demonstrate the steps that a program needs to take run as a background daemon or service.

[[User:Markhobley|Markhobley]] 12:49, 29 March 2013 (UTC)
