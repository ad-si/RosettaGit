::2048 Game Task from RosettaCode.org
::Batch File Implementation

@echo off
setlocal enabledelayedexpansion
cls

:begin_game
	%== Set variables ==%
set "score=0"
set "won=0"
set "SUP_score=0"
for /l %%A in (1,1,4) do for /l %%B in (1,1,4) do set /a "X_%%A%%B=0"

call :addtile
call :addtile

	%== Main game loop ==%
:main_loop
	set "changed=0"
	call :display
	echo(
	echo Keys: WASD (Slide Movement^), N (New game^), P (Exit^)

	%== Get Keypress ==%
	set "key="
	for /f "delims=" %%? in ('xcopy /w "%~f0" "%~f0" 2^>nul') do if not defined key set "key=%%?"
	set "key=%key:~-1%"

	%== Process keypress ==%
	if /i "!key!"=="W" (
		for /l %%? in (1,1,4) do call :slide X_1%%? X_2%%? X_3%%? X_4%%?
	)
	if /i "!key!"=="A" (
		for /l %%? in (1,1,4) do call :slide X_%%?1 X_%%?2 X_%%?3 X_%%?4
	)
	if /i "!key!"=="S" (
		for /l %%? in (1,1,4) do call :slide X_4%%? X_3%%? X_2%%? X_1%%?
	)
	if /i "!key!"=="D" (
		for /l %%? in (1,1,4) do call :slide X_%%?4 X_%%?3 X_%%?2 X_%%?1
	)
	if /i "!key!"=="N" goto :begin_game
	if /i "!key!"=="P" exit /b

	%== Check if the board changed ==%
	if %changed% neq 0 call :addtile

	%== Check if already won ==%
	if %won% equ 1 (
		set "msg=Nice one... You WON^!^!"
		goto :gameover
	)

	%== Check for lose condition ==%
	set /a "real_blanks=blank_count-1"
	if %real_blanks% leq 0 (
		for /l %%A in (1,1,4) do for /l %%B in (1,1,4) do set "TRY_%%A%%B=!X_%%A%%B!"
		set "TRY_changed=%changed%" & set "changed=0"
		set "SUP_score=1"
		for /l %%? in (1,1,4) do call :slide TRY_%%?1 TRY_%%?2 TRY_%%?3 TRY_%%?4
		for /l %%? in (1,1,4) do call :slide TRY_1%%? TRY_2%%? TRY_3%%? TRY_4%%?
		if !changed! equ 0 (
			set "msg=No moves are possible... Game Over :("
			goto :gameover
		) else (set "changed=!TRY_changed!" & set "SUP_score=0")
	)
goto main_loop


::~~~~~~~~~~~~~~~~~~~~ Sub Procedures ~~~~~~~~~~~~~~~~~~~~::
	%== Game Over xD ==%
:gameover
call :display
echo(
echo(!msg!
echo(
echo(Keys: N (New game^), P (Exit^)

:key_loop
set "key="
for /f "delims=" %%? in ('xcopy /w "%~f0" "%~f0" 2^>nul') do if not defined key set "key=%%?"
set "key=%key:~-1%"
if /i "!key!"=="N" goto :begin_game
if /i "!key!"=="P" exit /b
goto :key_loop

	%== The main slider of numbers in tiles ==%
:slide
set "next="
set "slide_1="
set "slide_2="
for %%? in (%*) do if !%%?! neq 0 set "slide_1=!slide_1! !%%?!"
for %%? in (!slide_1!) do (
	set "scan=%%?"
	if "!scan!"=="!next!" (
		set /a "next*=2"
		if !SUP_score! equ 0 set /a "score+=!next!"
		%== WINNING CONDITION!!! ==%
		if "!next!" equ "2048" set "won=1"
		set "scan="
	)
	set "slide_2=!slide_2! !next!"
	set "next=!scan!"
)
set "slide_2=!slide_2! !next!"
for /l %%? in (1,1,4) do set "final_%%?=0"
set "cnt=0" & for %%? in (!slide_2!) do if !cnt! lss 4 (
	set /a "cnt+=1"
	set "final_!cnt!=%%?"
)
if not "!%1!!%2!!%3!!%4!"=="!final_1!!final_2!!final_3!!final_4!" set "changed=1"
set "cnt=0" & for %%? in (%*) do (
	set /a "cnt+=1"
	set /a "%%?=final_!cnt!"
)
goto :EOF

	%== Add number to tile ==%
:addtile
set "blank_list="
set "blank_count=0"
for /l %%A in (1,1,4) do for /l %%B in (1,1,4) do (
	if !X_%%A%%B! equ 0 (
		set "blank_list=!blank_list!X_%%A%%B"
		set /a blank_count+=1
	)
)
set /a "pick_tile=(%random% %% %blank_count%)*4"
set /a "rnd=%random%%%10+1"
set "tile_new=!blank_list:~%pick_tile%,4!"
if %rnd%==5 (set !tile_new!=4) else (set !tile_new!=2)
goto :EOF

	%== Display the table ==%
:display
cls
echo 2048 Game in Batch
echo(
for /l %%A in (1,1,4) do (
	for /l %%B in (1,1,4) do (
		set "DX_%%A%%B=!X_%%A%%B!"
		if !tile_new!==X_%%A%%B (set "DX_%%A%%B=  +!X_%%A%%B!") else (
			if !X_%%A%%B! lss 1000 set "DX_%%A%%B= !DX_%%A%%B!"
			if !X_%%A%%B! lss 100 set "DX_%%A%%B= !DX_%%A%%B!"
			if !X_%%A%%B! lss 10 set "DX_%%A%%B= !DX_%%A%%B!"
			if !X_%%A%%B! equ 0 set "DX_%%A%%B=    "
		)
	)
		echo +----+----+----+----+
		echo ^|!DX_%%A1!^|!DX_%%A2!^|!DX_%%A3!^|!DX_%%A4!^|
)
echo +----+----+----+----+
echo(
echo Score: %score%
goto :EOF
