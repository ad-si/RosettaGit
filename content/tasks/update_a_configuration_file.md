+++
title = "Update a configuration file"
description = ""
date = 2018-12-02T07:25:17Z
aliases = []
[extra]
id = 9733
[taxonomies]
categories = ["task", "File handling"]
tags = []
+++

## Task

We have a configuration file as follows:

 # This is a configuration file in standard configuration file format
 #
 # Lines begininning with a hash or a semicolon are ignored by the application
 # program. Blank lines are also ignored by the application program.

 # The first word on each non comment line is the configuration option.
 # Remaining words or numbers on the line are configuration parameter
 # data fields.

 # Note that configuration option names are not case sensitive. However,
 # configuration parameter data is case sensitive and the lettercase must
 # be preserved.

 # This is a favourite fruit
 FAVOURITEFRUIT banana

 # This is a boolean that should be set
 NEEDSPEELING

 # This boolean is commented out
 ; SEEDSREMOVED

 # How many bananas we have
 NUMBEROFBANANAS 48

The task is to manipulate the configuration file as follows:

* Disable the needspeeling option (using a semicolon prefix)
* Enable the seedsremoved option by removing the semicolon and any leading whitespace
* Change the numberofbananas parameter to 1024
* Enable (or create if it does not exist in the file) a parameter for numberofstrawberries with a value of 62000



Note that configuration option names are not case sensitive. This means that changes should be effected, regardless of the case.

Options should always be disabled by prefixing them with a semicolon.

Lines beginning with hash symbols should not be manipulated and left unchanged in the revised file.

If a configuration option does not exist within the file (in either enabled or disabled form), it should be added during this update. Duplicate configuration option names in the file should be removed, leaving just the first entry.

For the purpose of this task, the revised file should contain appropriate entries, whether enabled or not for needspeeling,seedsremoved,numberofbananas and numberofstrawberries.)

The update should rewrite configuration option names in capital letters. However lines beginning with hashes and any parameter data must not be altered (eg the banana for favourite fruit must not become capitalized). The update process should also replace double semicolon prefixes with just a single semicolon (unless it is uncommenting the option, in which case it should remove all leading semicolons).

Any lines beginning with a semicolon or groups of semicolons, but no following option should be removed, as should any leading or trailing whitespace on the lines. Whitespace between the option and parameters should consist only of a single
space, and any non-ASCII extended characters, tabs characters, or control codes
(other than end of line markers), should also be removed.


## Related tasks

* [[Read a configuration file]]





## AutoHotkey


```AutoHotkey
; Author: AlephX, Aug 17 2011
data = %A_scriptdir%\rosettaconfig.txt
outdata = %A_scriptdir%\rosettaconfig.tmp
FileDelete, %outdata%

NUMBEROFBANANAS := 1024
numberofstrawberries := 560
NEEDSPEELING = "0"
FAVOURITEFRUIT := "bananas"
SEEDSREMOVED = "1"
BOOL0 = "0"
BOOL1 = "1"
NUMBER1 := 1
number0 := 0
STRINGA := "string here"

parameters = bool0|bool1|NUMBER1|number0|stringa|NEEDSPEELING|seedsremoved|numberofbananas|numberofstrawberries

Loop, Read, %data%, %outdata%
	{
	if (instr(A_LoopReadLine, "#") == 1 OR A_LoopReadLine == "")
		{
		Line := A_LoopReadLine
		}
	else
		{
		if instr(A_LoopReadLine, ";") == 1
			{
			parameter := RegExReplace(Substr(A_LoopReadLine,2), "^[ \s]+|[ \s]+$", "")

			parametervalue = %parameter%
			value := %parametervalue%
			if value == 0
				Line := A_loopReadLine
			else
				Line := Parameter
			}
		else
			{
			parameter := RegExReplace(A_LoopReadLine, "^[ \s]+|[ \s]+$", "")
			if instr(parameter, A_Space)
				parameter := substr(parameter, 1, instr(parameter, A_Space)-1)

			if instr(parameters, parameter) > 0
				{
				parametervalue = %parameter%
				value := %parametervalue%

				if (value = chr(34) . "0" . chr(34))
					Line := "; " . Parameter
				else
					{
					if (value = chr(34) . "1" . chr(34))
						Line := Parameter
					else
						Line = %parametervalue% %value%
					}
				}
			else
				Line := A_LoopReadLine
			}

		}
	StringReplace, parameters, parameters, %parametervalue%,,
	StringReplace, parameters, parameters,||,|

	FileAppend, %Line%`n
	}

Loop, parse, parameters,|
	{
	if (A_Loopfield <> "")
		{
		StringUpper, parameter, A_LoopField
		parametervalue = %parameter%
		value := %parametervalue%

		if (value = chr(34) . "0" . chr(34))
			Line := "; " . parameter
		else
			{
			if (value = chr(34) . "1" . chr(34))
				Line := parameter
			else
				Line = %parametervalue% %value%
			}

		FileAppend, %Line%`n, %outdata%
		}
	}

FileCopy, %A_scriptdir%\rosettaconfig.tmp, %A_scriptdir%\rosettaconfig.txt, 1
```



## BASIC

This program is not tied to the task requested, but it can read and modify ANY configuration file. It is somewhat long, but includes functionality to modify or add variables and values to the configuration file, append remarks (#) to it, read and save values in an array (comma separated), toggle comment mode for variables in a configuration file, etc. It is even longer because is almost fully commented and in key procedures every parameter is explained. It includes a main program cycle to read the configuration file and modify its values.

```qbasic

' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
' Read a Configuration File V1.0                    '
'                                                   '
' Developed by A. David Garza Marín in VB-DOS for   '
' RosettaCode. December 2, 2016.                    '
' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '

' OPTION EXPLICIT  ' For VB-DOS, PDS 7.1
' OPTION _EXPLICIT  ' For QB64

' SUBs and FUNCTIONs
DECLARE SUB AppendCommentToConfFile (WhichFile AS STRING, WhichComment AS STRING, LeaveALine AS INTEGER)
DECLARE SUB setNValToVarArr (WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS DOUBLE)
DECLARE SUB setSValToVar (WhichVariable AS STRING, WhatValue AS STRING)
DECLARE SUB setSValToVarArr (WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS STRING)
DECLARE SUB doModifyArrValueFromConfFile (WhichFile AS STRING, WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS STRING, Separator AS STRING, ToComment AS INTEGER)
DECLARE SUB doModifyValueFromConfFile (WhichFile AS STRING, WhichVariable AS STRING, WhatValue AS STRING, Separator AS STRING, ToComment AS INTEGER)
DECLARE FUNCTION CreateConfFile% (WhichFile AS STRING)
DECLARE FUNCTION ErrorMessage$ (WhichError AS INTEGER)
DECLARE FUNCTION FileExists% (WhichFile AS STRING)
DECLARE FUNCTION FindVarPos% (WhichVariable AS STRING)
DECLARE FUNCTION FindVarPosArr% (WhichVariable AS STRING, WhichIndex AS INTEGER)
DECLARE FUNCTION getArrayVariable$ (WhichVariable AS STRING, WhichIndex AS INTEGER)
DECLARE FUNCTION getVariable$ (WhichVariable AS STRING)
DECLARE FUNCTION getVarType% (WhatValue AS STRING)
DECLARE FUNCTION GetDummyFile$ (WhichFile AS STRING)
DECLARE FUNCTION HowManyElementsInTheArray% (WhichVariable AS STRING)
DECLARE FUNCTION IsItAnArray% (WhichVariable AS STRING)
DECLARE FUNCTION IsItTheVariableImLookingFor% (TextToAnalyze AS STRING, WhichVariable AS STRING)
DECLARE FUNCTION NewValueForTheVariable$ (WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS STRING, Separator AS STRING)
DECLARE FUNCTION ReadConfFile% (NameOfConfFile AS STRING)
DECLARE FUNCTION YorN$ ()

' Register for values located
TYPE regVarValue
  VarName AS STRING * 20
  VarType AS INTEGER ' 1=String, 2=Integer, 3=Real, 4=Comment
  VarValue AS STRING * 30
END TYPE

' Var
DIM rVarValue() AS regVarValue, iErr AS INTEGER, i AS INTEGER, iHMV AS INTEGER
DIM iArrayElements AS INTEGER, iWhichElement AS INTEGER, iCommentStat AS INTEGER
DIM iAnArray AS INTEGER, iSave AS INTEGER
DIM otherfamily(1 TO 2) AS STRING
DIM sVar AS STRING, sVal AS STRING, sComment AS STRING
CONST ConfFileName = "config2.fil"
CONST False = 0, True = NOT False

' ------------------- Main Program ------------------------
DO
  CLS
  ERASE rVarValue
  PRINT "This program reads a configuration file and shows the result."
  PRINT
  PRINT "Default file name: "; ConfFileName
  PRINT
  iErr = ReadConfFile(ConfFileName)
  IF iErr = 0 THEN
    iHMV = UBOUND(rVarValue)
    PRINT "Variables found in file:"
    FOR i = 1 TO iHMV
      PRINT RTRIM$(rVarValue(i).VarName); " = "; RTRIM$(rVarValue(i).VarValue); " (";
      SELECT CASE rVarValue(i).VarType
        CASE 0: PRINT "Undefined";
        CASE 1: PRINT "String";
        CASE 2: PRINT "Integer";
        CASE 3: PRINT "Real";
        CASE 4: PRINT "Is a commented variable";
      END SELECT
      PRINT ")"
    NEXT i
    PRINT

    INPUT "Type the variable name to modify (Blank=End)"; sVar
    sVar = RTRIM$(LTRIM$(sVar))
    IF LEN(sVar) > 0 THEN
       i = FindVarPos%(sVar)
       IF i > 0 THEN ' Variable found
         iAnArray = IsItAnArray%(sVar)
         IF iAnArray THEN
           iArrayElements = HowManyElementsInTheArray%(sVar)
           PRINT "This is an array of"; iArrayElements; " elements."
           INPUT "Which one do you want to modify (Default=1)"; iWhichElement
           IF iWhichElement = 0 THEN iWhichElement = 1
         ELSE
           iArrayElements = 1
           iWhichElement = 1
         END IF
         PRINT "The current value of the variable is: "
         IF iAnArray THEN
           PRINT sVar; "("; iWhichElement; ") = "; RTRIM$(rVarValue(i + (iWhichElement - 1)).VarValue)
         ELSE
           PRINT sVar; " = "; RTRIM$(rVarValue(i + (iWhichElement - 1)).VarValue)
         END IF
       ELSE
         PRINT "The variable was not found. It will be added."
       END IF
       PRINT
       INPUT "Please, set the new value for the variable (Blank=Unmodified)"; sVal
       sVal = RTRIM$(LTRIM$(sVal))
       IF i > 0 THEN
         IF rVarValue(i + (iWhichElement - 1)).VarType = 4 THEN
           PRINT "Do you want to remove the comment status to the variable? (Y/N)"
           iCommentStat = NOT (YorN = "Y")
           iCommentStat = ABS(iCommentStat) ' Gets 0 (Toggle) or 1 (Leave unmodified)
           iSave = (iCommentStat = 0)
         ELSE
           PRINT "Do you want to toggle the variable as a comment? (Y/N)"
           iCommentStat = (YorN = "Y") ' Gets 0 (Uncommented) or -1 (Toggle as a Comment)
           iSave = iCommentStat
         END IF
       END IF

       ' Now, update or add the variable to the conf file
       IF i > 0 THEN
         IF sVal = "" THEN
           sVal = RTRIM$(rVarValue(i).VarValue)
         END IF
       ELSE
         PRINT "The variable will be added to the configuration file."
         PRINT "Do you want to add a remark for it? (Y/N)"
         IF YorN$ = "Y" THEN
           LINE INPUT "Please, write your remark: ", sComment
           sComment = LTRIM$(RTRIM$(sComment))
           IF sComment <> "" THEN
             AppendCommentToConfFile ConfFileName, sComment, True
           END IF
         END IF
       END IF

       ' Verifies if the variable will be modified, and applies the modification
       IF sVal <> "" OR iSave THEN
         IF iWhichElement > 1 THEN
           setSValToVarArr sVar, iWhichElement, sVal
           doModifyArrValueFromConfFile ConfFileName, sVar, iWhichElement, sVal, " ", iCommentStat
         ELSE
           setSValToVar sVar, sVal
           doModifyValueFromConfFile ConfFileName, sVar, sVal, " ", iCommentStat
         END IF
       END IF

    END IF
  ELSE
    PRINT ErrorMessage$(iErr)
  END IF
  PRINT
  PRINT "Do you want to add or modify another variable? (Y/N)"
LOOP UNTIL YorN$ = "N"
' --------- End of Main Program -----------------------
PRINT
PRINT "End of program."
END

FileError:
  iErr = ERR
RESUME NEXT

SUB AppendCommentToConfFile (WhichFile AS STRING, WhichComment AS STRING, LeaveALine AS INTEGER)
   ' Parameters:
   ' WhichFile: Name of the file where a comment will be appended.
   ' WhichComment: A comment. It is suggested to add a comment no larger than 75 characters.
   '               This procedure adds a # at the beginning of the string if there is no #
   '               sign on it in order to ensure it will be added as a comment.

   ' Var
   DIM iFil AS INTEGER

   iFil = FileExists%(WhichFile)
   IF NOT iFil THEN
     iFil = CreateConfFile%(WhichFile)  ' Here, iFil is used as dummy to save memory
   END IF

   IF iFil THEN  ' Everything is Ok
     iFil = FREEFILE ' Now, iFil is used to be the ID of the file
     WhichComment = LTRIM$(RTRIM$(WhichComment))

     IF LEFT$(WhichComment, 1) <> "#" THEN  ' Is it in comment format?
       WhichComment = "# " + WhichComment
     END IF

     ' Append the comment to the file
     OPEN WhichFile FOR APPEND AS #iFil
       IF LeaveALine THEN
         PRINT #iFil, ""
       END IF
       PRINT #iFil, WhichComment
     CLOSE #iFil
   END IF

END SUB

FUNCTION CreateConfFile% (WhichFile AS STRING)
  ' Var
  DIM iFile AS INTEGER

  ON ERROR GOTO FileError

  iFile = FREEFILE
  OPEN WhichFile FOR OUTPUT AS #iFile
  CLOSE iFile

  ON ERROR GOTO 0

  CreateConfFile = FileExists%(WhichFile)
END FUNCTION

SUB doModifyArrValueFromConfFile (WhichFile AS STRING, WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS STRING, Separator AS STRING, ToComment AS INTEGER)
    ' Parameters:
    ' WhichFile: The name of the Configuration File. It can include the full path.
    ' WhichVariable: The name of the variable to be modified or added to the conf file.
    ' WhichIndex: The index number of the element to be modified in a matrix (Default=1)
    ' WhatValue: The new value to set in the variable specified in WhichVariable.
    ' Separator: The separator between the variable name and its value in the conf file. Defaults to a space " ".
    ' ToComment: A value to set or remove the comment mode of a variable: -1=Toggle to Comment, 0=Toggle to not comment, 1=Leave as it is.

    ' Var
    DIM iFile AS INTEGER, iFile2 AS INTEGER, iError AS INTEGER
    DIM iMod AS INTEGER, iIsComment AS INTEGER
    DIM sLine AS STRING, sDummyFile AS STRING, sChar AS STRING

    ' If conf file doesn't exists, create one.
    iError = 0
    iMod = 0
    IF NOT FileExists%(WhichFile) THEN
      iError = CreateConfFile%(WhichFile)
    END IF

    IF NOT iError THEN  ' File exists or it was created
      Separator = RTRIM$(LTRIM$(Separator))
      IF Separator = "" THEN
        Separator = " "  ' Defaults to Space
      END IF
      sDummyFile = GetDummyFile$(WhichFile)

      ' It is assumed a text file
      iFile = FREEFILE
      OPEN WhichFile FOR INPUT AS #iFile

      iFile2 = FREEFILE
      OPEN sDummyFile FOR OUTPUT AS #iFile2

      ' Goes through the file to find the variable
      DO WHILE NOT EOF(iFile)
        LINE INPUT #iFile, sLine
        sLine = RTRIM$(LTRIM$(sLine))
        sChar = LEFT$(sLine, 1)
        iIsComment = (sChar = ";")
        IF iIsComment THEN  ' Variable is commented
          sLine = LTRIM$(MID$(sLine, 2))
        END IF

        IF sChar <> "#" AND LEN(sLine) > 0 THEN ' Is not a comment?
          IF IsItTheVariableImLookingFor%(sLine, WhichVariable) THEN
             sLine = NewValueForTheVariable$(WhichVariable, WhichIndex, WhatValue, Separator)
             iMod = True
             IF ToComment = True THEN
               sLine = "; " + sLine
             END IF
          ELSEIF iIsComment THEN
             sLine = "; " + sLine
          END IF

        END IF

        PRINT #iFile2, sLine
      LOOP

      ' Reviews if a modification was done, if not, then it will
      '   add the variable to the file.
      IF NOT iMod THEN
        sLine = NewValueForTheVariable$(WhichVariable, 1, WhatValue, Separator)
        PRINT #iFile2, sLine
      END IF
      CLOSE iFile2, iFile

      ' Removes the conf file and sets the dummy file as the conf file
      KILL WhichFile
      NAME sDummyFile AS WhichFile
    END IF

END SUB

SUB doModifyValueFromConfFile (WhichFile AS STRING, WhichVariable AS STRING, WhatValue AS STRING, Separator AS STRING, ToComment AS INTEGER)
  ' To see details of parameters, please see doModifyArrValueFromConfFile
  doModifyArrValueFromConfFile WhichFile, WhichVariable, 1, WhatValue, Separator, ToComment
END SUB

FUNCTION ErrorMessage$ (WhichError AS INTEGER)
    ' Var
    DIM sError AS STRING

    SELECT CASE WhichError
      CASE 0: sError = "Everything went ok."
      CASE 1: sError = "Configuration file doesn't exist."
      CASE 2: sError = "There are no variables in the given file."
    END SELECT

    ErrorMessage$ = sError
END FUNCTION

FUNCTION FileExists% (WhichFile AS STRING)
    ' Var
    DIM iFile AS INTEGER
    DIM iItExists AS INTEGER
    SHARED iErr AS INTEGER

    ON ERROR GOTO FileError
    iFile = FREEFILE
    iErr = 0
    OPEN WhichFile FOR BINARY AS #iFile
    IF iErr = 0 THEN
        iItExists = LOF(iFile) > 0
        CLOSE #iFile

        IF NOT iItExists THEN
            KILL WhichFile
        END IF
    END IF
    ON ERROR GOTO 0
    FileExists% = iItExists

END FUNCTION

FUNCTION FindVarPos% (WhichVariable AS STRING)
  ' Will find the position of the variable
  FindVarPos% = FindVarPosArr%(WhichVariable, 1)
END FUNCTION

FUNCTION FindVarPosArr% (WhichVariable AS STRING, WhichIndex AS INTEGER)
  ' Var
  DIM i AS INTEGER, iHMV AS INTEGER, iCount AS INTEGER, iPos AS INTEGER
  DIM sVar AS STRING, sVal AS STRING, sWV AS STRING
  SHARED rVarValue() AS regVarValue

  ' Looks for a variable name and returns its position
  iHMV = UBOUND(rVarValue)
  sWV = UCASE$(LTRIM$(RTRIM$(WhichVariable)))
  sVal = ""
  iCount = 0
  DO
    i = i + 1
    sVar = UCASE$(RTRIM$(rVarValue(i).VarName))
    IF sVar = sWV THEN
      iCount = iCount + 1
      IF iCount = WhichIndex THEN
        iPos = i
      END IF
    END IF
  LOOP UNTIL i >= iHMV OR iPos > 0

  FindVarPosArr% = iPos
END FUNCTION

FUNCTION getArrayVariable$ (WhichVariable AS STRING, WhichIndex AS INTEGER)
  ' Var
  DIM i AS INTEGER
  DIM sVal AS STRING
  SHARED rVarValue() AS regVarValue

  i = FindVarPosArr%(WhichVariable, WhichIndex)
  sVal = ""
  IF i > 0 THEN
    sVal = RTRIM$(rVarValue(i).VarValue)
  END IF

  ' Found it or not, it will return the result.
  ' If the result is "" then it didn't found the requested variable.
  getArrayVariable$ = sVal

END FUNCTION

FUNCTION GetDummyFile$ (WhichFile AS STRING)
    ' Var
    DIM i AS INTEGER, j AS INTEGER

    ' Gets the path specified in WhichFile
    i = 1
    DO
        j = INSTR(i, WhichFile, "\")
        IF j > 0 THEN i = j + 1
    LOOP UNTIL j = 0

    GetDummyFile$ = LEFT$(WhichFile, i - 1) + "$dummyf$.tmp"
END FUNCTION

FUNCTION getVariable$ (WhichVariable AS STRING)
  ' Var
  DIM i AS INTEGER, iHMV AS INTEGER
  DIM sVal AS STRING

  ' For a single variable, looks in the first (and only)
  '   element of the array that contains the name requested.
  sVal = getArrayVariable$(WhichVariable, 1)

  getVariable$ = sVal
END FUNCTION

FUNCTION getVarType% (WhatValue AS STRING)
  ' Var
  DIM sValue AS STRING, dValue AS DOUBLE, iType AS INTEGER

  sValue = RTRIM$(WhatValue)
  iType = 0
  IF LEN(sValue) > 0 THEN
    IF ASC(LEFT$(sValue, 1)) < 48 OR ASC(LEFT$(sValue, 1)) > 57 THEN
      iType = 1  ' String
    ELSE
      dValue = VAL(sValue)
      IF CLNG(dValue) = dValue THEN
        iType = 2 ' Integer
      ELSE
        iType = 3 ' Real
      END IF
    END IF
  END IF

  getVarType% = iType
END FUNCTION

FUNCTION HowManyElementsInTheArray% (WhichVariable AS STRING)
  ' Var
  DIM i AS INTEGER, iHMV AS INTEGER, iCount AS INTEGER, iPos AS INTEGER, iQuit AS INTEGER
  DIM sVar AS STRING, sVal AS STRING, sWV AS STRING
  SHARED rVarValue() AS regVarValue

  ' Looks for a variable name and returns its value
  iHMV = UBOUND(rVarValue)
  sWV = UCASE$(LTRIM$(RTRIM$(WhichVariable)))
  sVal = ""

  ' Look for all instances of WhichVariable in the
  '  list. This is because elements of an array will not alwasy
  '  be one after another, but alternate.
  FOR i = 1 TO iHMV
    sVar = UCASE$(RTRIM$(rVarValue(i).VarName))
    IF sVar = sWV THEN
      iCount = iCount + 1
    END IF
  NEXT i

  HowManyElementsInTheArray = iCount
END FUNCTION

FUNCTION IsItAnArray% (WhichVariable AS STRING)
  ' Returns if a Variable is an Array
  IsItAnArray% = (HowManyElementsInTheArray%(WhichVariable) > 1)

END FUNCTION

FUNCTION IsItTheVariableImLookingFor% (TextToAnalyze AS STRING, WhichVariable AS STRING)
  ' Var
  DIM sVar AS STRING, sDT AS STRING, sDV AS STRING
  DIM iSep AS INTEGER

  sDT = UCASE$(RTRIM$(LTRIM$(TextToAnalyze)))
  sDV = UCASE$(RTRIM$(LTRIM$(WhichVariable)))
  iSep = INSTR(sDT, "=")
  IF iSep = 0 THEN iSep = INSTR(sDT, " ")
  IF iSep > 0 THEN
    sVar = RTRIM$(LEFT$(sDT, iSep - 1))
  ELSE
    sVar = sDT
  END IF

  ' It will return True or False
  IsItTheVariableImLookingFor% = (sVar = sDV)
END FUNCTION

FUNCTION NewValueForTheVariable$ (WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS STRING, Separator AS STRING)
  ' Var
  DIM iItem AS INTEGER, iItems AS INTEGER, iFirstItem AS INTEGER
  DIM i AS INTEGER, iCount AS INTEGER, iHMV AS INTEGER
  DIM sLine AS STRING, sVar AS STRING, sVar2 AS STRING
  SHARED rVarValue() AS regVarValue

  IF IsItAnArray%(WhichVariable) THEN
    iItems = HowManyElementsInTheArray(WhichVariable)
    iFirstItem = FindVarPosArr%(WhichVariable, 1)
  ELSE
    iItems = 1
    iFirstItem = FindVarPos%(WhichVariable)
  END IF
  iItem = FindVarPosArr%(WhichVariable, WhichIndex)
  sLine = ""
  sVar = UCASE$(WhichVariable)
  iHMV = UBOUND(rVarValue)

  IF iItem > 0 THEN
    i = iFirstItem
    DO
      sVar2 = UCASE$(RTRIM$(rVarValue(i).VarName))

      IF sVar = sVar2 THEN  ' Does it found an element of the array?
        iCount = iCount + 1
        IF LEN(sLine) > 0 THEN ' Add a comma
          sLine = sLine + ", "
        END IF
        IF i = iItem THEN
          sLine = sLine + WhatValue
        ELSE
          sLine = sLine + RTRIM$(rVarValue(i).VarValue)
        END IF
      END IF
      i = i + 1
    LOOP UNTIL i > iHMV OR iCount = iItems

    sLine = WhichVariable + Separator + sLine
  ELSE
    sLine = WhichVariable + Separator + WhatValue
  END IF

  NewValueForTheVariable$ = sLine
END FUNCTION

FUNCTION ReadConfFile% (NameOfConfFile AS STRING)
  ' Var
  DIM iFile AS INTEGER, iType AS INTEGER, iVar AS INTEGER, iHMV AS INTEGER
  DIM iVal AS INTEGER, iCurVar AS INTEGER, i AS INTEGER, iErr AS INTEGER
  DIM dValue AS DOUBLE, iIsComment AS INTEGER
  DIM sLine AS STRING, sVar AS STRING, sValue  AS STRING
  SHARED rVarValue() AS regVarValue

  ' This procedure reads a configuration file with variables
  '  and values separated by the equal sign (=) or a space.
  '  It needs the FileExists% function.
  '  Lines begining with # or blank will be ignored.
  IF FileExists%(NameOfConfFile) THEN
    iFile = FREEFILE
    REDIM rVarValue(1 TO 10) AS regVarValue
    OPEN NameOfConfFile FOR INPUT AS #iFile
      WHILE NOT EOF(iFile)
        LINE INPUT #iFile, sLine
        sLine = RTRIM$(LTRIM$(sLine))
        IF LEN(sLine) > 0 THEN ' Does it have any content?
          IF LEFT$(sLine, 1) <> "#" THEN   ' Is not a comment?
            iIsComment = (LEFT$(sLine, 1) = ";")
            IF iIsComment THEN ' It is a commented variable
              sLine = LTRIM$(MID$(sLine, 2))
            END IF
            iVar = INSTR(sLine, "=")  ' Is there an equal sign?
            IF iVar = 0 THEN iVar = INSTR(sLine, " ") ' if not then is there a space?

            GOSUB AddASpaceForAVariable
            iCurVar = iHMV
            IF iVar > 0 THEN  ' Is a variable and a value
              rVarValue(iHMV).VarName = LEFT$(sLine, iVar - 1)
            ELSE              ' Is just a variable name
              rVarValue(iHMV).VarName = sLine
              rVarValue(iHMV).VarValue = ""
            END IF

            IF iVar > 0 THEN  ' Get the value(s)
              sLine = LTRIM$(MID$(sLine, iVar + 1))
              DO  ' Look for commas
                iVal = INSTR(sLine, ",")
                IF iVal > 0 THEN  ' There is a comma
                  rVarValue(iHMV).VarValue = RTRIM$(LEFT$(sLine, iVal - 1))
                  GOSUB AddASpaceForAVariable
                  rVarValue(iHMV).VarName = rVarValue(iHMV - 1).VarName  ' Repeats the variable name
                  sLine = LTRIM$(MID$(sLine, iVal + 1))
                END IF
              LOOP UNTIL iVal = 0
              rVarValue(iHMV).VarValue = sLine

            END IF

            ' Determine the variable type of each variable found in this step
            FOR i = iCurVar TO iHMV
              IF iIsComment THEN
                rVarValue(i).VarType = 4  ' Is a comment
              ELSE
                GOSUB DetermineVariableType
              END IF
            NEXT i

          END IF
        END IF
      WEND
    CLOSE iFile
    IF iHMV > 0 THEN
      REDIM PRESERVE rVarValue(1 TO iHMV) AS regVarValue
      iErr = 0 ' Everything ran ok.
    ELSE
      REDIM rVarValue(1 TO 1) AS regVarValue
      iErr = 2 ' No variables found in configuration file
    END IF
  ELSE
    iErr = 1 ' File doesn't exist
  END IF

  ReadConfFile = iErr

EXIT FUNCTION

AddASpaceForAVariable:
  iHMV = iHMV + 1

  IF UBOUND(rVarValue) < iHMV THEN  ' Are there space for a new one?
    REDIM PRESERVE rVarValue(1 TO iHMV + 9) AS regVarValue
  END IF
RETURN

DetermineVariableType:
  sValue = RTRIM$(rVarValue(i).VarValue)
  IF LEN(sValue) > 0 THEN
    IF ASC(LEFT$(sValue, 1)) < 48 OR ASC(LEFT$(sValue, 1)) > 57 THEN
      rVarValue(i).VarType = 1  ' String
    ELSE
      dValue = VAL(sValue)
      IF CLNG(dValue) = dValue THEN
        rVarValue(i).VarType = 2 ' Integer
      ELSE
        rVarValue(i).VarType = 3 ' Real
      END IF
    END IF
  END IF
RETURN

END FUNCTION

SUB setNValToVar (WhichVariable AS STRING, WhatValue AS DOUBLE)
  ' Sets a numeric value to a variable
  setNValToVarArr WhichVariable, 1, WhatValue
END SUB

SUB setNValToVarArr (WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS DOUBLE)
  ' Sets a numeric value to a variable array
  ' Var
  DIM sVal AS STRING
  sVal = FORMAT$(WhatValue)
  setSValToVarArr WhichVariable, WhichIndex, sVal
END SUB

SUB setSValToVar (WhichVariable AS STRING, WhatValue AS STRING)
   ' Sets a string value to a variable
   setSValToVarArr WhichVariable, 1, WhatValue
END SUB

SUB setSValToVarArr (WhichVariable AS STRING, WhichIndex AS INTEGER, WhatValue AS STRING)
  ' Sets a string value to a variable array
  ' Var
  DIM i AS INTEGER
  DIM sVar AS STRING
  SHARED rVarValue() AS regVarValue

  i = FindVarPosArr%(WhichVariable, WhichIndex)
  IF i = 0 THEN  ' Should add the variable
    IF UBOUND(rVarValue) > 0 THEN
      sVar = RTRIM$(rVarValue(1).VarName)
      IF sVar <> "" THEN
        i = UBOUND(rVarValue) + 1
        REDIM PRESERVE rVarValue(1 TO i) AS regVarValue
      ELSE
        i = 1
      END IF
    ELSE
      REDIM rVarValue(1 TO i) AS regVarValue
    END IF
    rVarValue(i).VarName = WhichVariable
  END IF

  ' Sets the new value to the variable
  rVarValue(i).VarValue = WhatValue
  rVarValue(i).VarType = getVarType%(WhatValue)
END SUB

FUNCTION YorN$ ()
    ' Var
    DIM sYorN AS STRING

    DO
      sYorN = UCASE$(INPUT$(1))
      IF INSTR("YN", sYorN) = 0 THEN
        BEEP
      END IF
    LOOP UNTIL sYorN = "Y" OR sYorN = "N"

    YorN$ = sYorN
END FUNCTION

```


In the following example, the user can modify the variables, their comment status and add the NUMBEROFSTRAWBERRIES variable with the value of 64000. In this case, the user is modifying the value of the NUMBEROFBANANAS variable in the configuration file.


```txt

This program reads a configuration file and shows the result.

Default file name: config.fil

Variables found in file:
FAVOURITEFRUIT = banana (String)
NEEDSPEELING = (Undefined)
SEEDSREMOVED = (Is a commented variable)
NUMBEROFBANANAS = 48 (Integer)

Type the variable name to modify (Blank=End)? numberofbananas
The current value of the variable is:
NUMBEROFBANANAS = 48

Please, set the new value for the variable (Blank=Unmodified)? 1024
Do you want to toggle the variable as a comment? (Y/N)

Do you want to add or modify another variable? (Y/N)

```



## C


C with POSIX <code>strcasecmp</code> function for case-insensitive comparing. Substitute your toolchain's version.


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define strcomp(X, Y) strcasecmp(X, Y)

struct option
{ const char *name, *value;
  int flag; };

/* TODO: dynamically obtain these */
struct option updlist[] =
{ { "NEEDSPEELING", NULL },
  { "SEEDSREMOVED", "" },
  { "NUMBEROFBANANAS", "1024" },
  { "NUMBEROFSTRAWBERRIES", "62000" },
  { NULL, NULL } };

int output_opt(FILE *to, struct option *opt)
{ if (opt->value == NULL)
    return fprintf(to, "; %s\n", opt->name);
  else if (opt->value[0] == 0)
    return fprintf(to, "%s\n", opt->name);
  else
    return fprintf(to, "%s %s\n", opt->name, opt->value); }

int update(FILE *from, FILE *to, struct option *updlist)
{ char line_buf[256], opt_name[128];
  int i;
  for (;;)
  { size_t len, space_span, span_to_hash;
    if (fgets(line_buf, sizeof line_buf, from) == NULL)
      break;
    len = strlen(line_buf);
    space_span = strspn(line_buf, "\t ");
    span_to_hash = strcspn(line_buf, "#");
    if (space_span == span_to_hash)
      goto line_out;
    if (space_span == len)
      goto line_out;
    if ((sscanf(line_buf, "; %127s", opt_name) == 1) ||
        (sscanf(line_buf, "%127s", opt_name) == 1))
    { int flag = 0;
      for (i = 0; updlist[i].name; i++)
      { if (strcomp(updlist[i].name, opt_name) == 0)
        { if (output_opt(to, &updlist[i]) < 0)
            return -1;
          updlist[i].flag = 1;
          flag = 1; } }
      if (flag == 0)
        goto line_out; }
    else
  line_out:
      if (fprintf(to, "%s", line_buf) < 0)
        return -1;
    continue; }
  { for (i = 0; updlist[i].name; i++)
    { if (!updlist[i].flag)
        if (output_opt(to, &updlist[i]) < 0)
          return -1; } }
  return feof(from) ? 0 : -1; }

int main(void)
{ if (update(stdin, stdout, updlist) < 0)
  { fprintf(stderr, "failed\n");
    return (EXIT_FAILURE); }
  return 0; }
```


Run:


```txt
$ ./a.out  < configfile
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000
```



## D

This type of file is really not very suitable for automated management, so this code is very basic.

```d
import std.stdio, std.file, std.string, std.regex, std.path,
       std.typecons;

final class Config {
    enum EntryType { empty, enabled, disabled, comment, ignore }

    static protected struct Entry {
        EntryType type;
        string name, value;
    }
    protected Entry[] entries;
    protected string path;

    this(in string path) {
        if (!isValidPath(path) || (exists(path) && !isFile(path)))
            throw new Exception("Invalid filename");

        this.path = path;
        if (!exists(path))
            return;

        auto r = regex(r"^(;*)\s*([A-Z0-9]+)\s*([A-Z0-9]*)", "i");
        auto f = File(path, "r");
        foreach (const buf; f.byLine()) {
            auto line = buf.strip().idup;
            if (!line.length)
                entries ~= Entry(EntryType.empty);
            else if (line[0] == '#')
                entries ~= Entry(EntryType.comment, line);
            else {
                line = line.removechars("^a-zA-Z0-9\x20;");
                auto m = match(line, r);
                if (!m.empty && m.front[2].length) {
                    EntryType t = EntryType.enabled;
                    if (m.front[1].length)
                        t = EntryType.disabled;
                    addOption(m.front[2], m.front[3], t);
                }
            }
        }
    }

    void enableOption(in string name) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].type = EntryType.enabled;
    }

    void disableOption(in string name) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].type = EntryType.disabled;
    }

    void setOption(in string name, in string value) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].value = value;
    }

    void addOption(in string name, in string val,
                   in EntryType t = EntryType.enabled) pure {
        entries ~= Entry(t, name.toUpper(), val);
    }

    void removeOption(in string name) pure {
        immutable i = getOptionIndex(name);
        if (!i.isNull)
            entries[i].type = EntryType.ignore;
    }

    Nullable!size_t getOptionIndex(in string name) const pure {
        foreach (immutable i, const ref e; entries) {
            if (e.type != EntryType.enabled &&
                e.type != EntryType.disabled)
                continue;
            if (e.name == name.toUpper())
                return typeof(return)(i);
        }
        return typeof(return).init;
    }

    void store() {
        auto f = File(path, "w+");
        foreach (immutable e; entries) {
            final switch (e.type) {
                case EntryType.empty:
                    f.writeln();
                    break;
                case EntryType.enabled:
                    f.writefln("%s %s", e.name, e.value);
                    break;
                case EntryType.disabled:
                    f.writefln("; %s %s", e.name, e.value);
                    break;
                case EntryType.comment:
                    f.writeln(e.name);
                    break;
                case EntryType.ignore:
                    continue;
            }
        }
    }
}

void main() {
    auto cfg = new Config("config.txt");
    cfg.enableOption("seedsremoved");
    cfg.disableOption("needspeeling");
    cfg.setOption("numberofbananas", "1024");
    cfg.addOption("numberofstrawberries", "62000");
    cfg.store();
}
```

Input file:

```txt
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
    # be preserved.

# This is a favourite fruit
FAVOURITEFRUIT    ßßßßß		banana    µµµµ

# This is a boolean that should be set
NEEDspeeling

# This boolean is commented out
;;;; SEEDSREMOVED µµµµµ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

# How many bananas we have
    NUMBEROFBANANAS µµµµµ 48
```

Output file:

```txt
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000
```



## Erlang

Given the large number of very exact rules governing the update of this configuration file it is with some pleasure I add new options to the beginning of the file.

```Erlang

-module( update_configuration_file ).

-export( [add/3, change/3, disable/2, enable/2, read/1, task/0, write/2] ).

add( Option, Value, Lines ) ->
	Upper = string:to_upper( Option ),
	[string:join( [Upper, Value], " " ) | Lines].

change( Option, Value, Lines ) ->
	Upper = string:to_upper( Option ),
	change_done( Option, Value, Lines, [change_option(Upper, Value, X) || X <- Lines] ).

disable( Option, Lines ) ->
	Upper = string:to_upper( Option ),
	[disable_option(Upper, X) || X <- Lines].

enable( Option, Lines ) ->
	Upper = string:to_upper( Option ),
	[enable_option(Upper, X) || X <- Lines].

read( Name ) ->
	{ok, Binary} = file:read_file( Name ),
	Lines = [binary:bin_to_list(X) || X <- binary:split( Binary, <<"\n">>, [global] )],
	Lines_no_white = [string:strip(X) || X <- Lines],
	Lines_no_control = [strip_control(X) || X <- Lines_no_white],
	Lines_no_consecutive_space = [string:join(string:tokens(X, " "), " ") || X <- Lines_no_control],
	Lines_no_consecutive_semicolon = [strip_semicolon(X) || X <- Lines_no_consecutive_space],
	Lines_no_empty = lists:filter( fun strip_empty/1, Lines_no_consecutive_semicolon ),
	Lines_upper = [to_upper(X) || X <- Lines_no_empty],
	lists:reverse( lists:foldl(fun remove_duplicates/2, [], Lines_upper) ).

task() ->
	Lines = read( "priv/configuration_file2" ),
	Disabled_lines = disable( "needspeeling", Lines ),
	Enabled_lines = enable( "SEEDSREMOVED", Disabled_lines ),
	Changed_lines1 = change( "NUMBEROFBANANAS", "1024", Enabled_lines ),
	Changed_lines2 = change( "numberofstrawberries", "62000", Changed_lines1 ),
	write( "configuration_file", Changed_lines2 ),
	[io:fwrite( "Wrote this line: ~s~n", [X]) || X <- Changed_lines2].

write( Name, Lines ) -> file:write_file( Name, binary:list_to_bin(string:join(Lines, "\n")) ).



change_done( Option, Value, Lines, Lines ) -> add( Option, Value, Lines );
change_done( _Option, _Value, _Lines, New_lines ) -> New_lines.

change_option( Option, Value, String ) -> change_option_same( string:str(String, Option), Value, String ).

change_option_same( 1, Value, String ) ->
	[Option | _T] = string:tokens( String, " " ),
	string:join( [Option, Value], " " );
change_option_same( _N, _Value, String ) -> String.

disable_option( Option, String ) -> disable_option_same( string:str(String, Option), String ).

disable_option_same( 1, String ) -> "; " ++ String;
disable_option_same( _N, String ) -> String.

enable_option( Option, String ) -> enable_option_same( string:str(String, "; " ++ Option), String ).

enable_option_same( 1, "; " ++ String ) -> String;
enable_option_same( _N, String ) -> String.

is_semicolon( $; ) -> true;
is_semicolon( _C ) -> false.

remove_duplicates( "#" ++_T=Line, Lines ) -> [Line | Lines];
remove_duplicates( Line, Lines ) ->
	Duplicates = [X || X <-Lines, 1 =:= string:str(Line, X)],
	remove_duplicates( Duplicates, Line, Lines ).

remove_duplicates( [], Line, Lines ) -> [Line | Lines];
remove_duplicates( _Duplicates, _Line, Lines ) -> Lines.

strip_control( "" ) -> "";
strip_control( ";" ++ _T=String ) -> lists:filter( fun strip_control_codes:is_not_control_code_nor_extended_character/1, String );
strip_control( String ) -> String.

strip_empty( ";" ) -> false;
strip_empty( _String ) -> true.


strip_semicolon( ";" ++ _T=String ) -> ";" ++ lists:dropwhile( fun is_semicolon/1, String );
strip_semicolon( String ) -> String.

to_upper( "" ) -> "";
to_upper( "#" ++ _T=String ) -> String;
to_upper( "; " ++ _T=String ) ->
	[";", Option | T] = string:tokens( String, " " ),
	string:join( [";", string:to_upper(Option) | T], " " );
to_upper( String ) ->
	[Option | T] = string:tokens( String, " " ),
	string:join( [string:to_upper(Option) | T], " " ).

```

```txt

51> update_configuration_file:task().
Wrote this line: NUMBEROFSTRAWBERRIES 62000
Wrote this line: # This is a configuration file in standard configuration file format
Wrote this line: #
Wrote this line: # Lines begininning with a hash or a semicolon are ignored by the application
Wrote this line: # program. Blank lines are also ignored by the application program.
Wrote this line:
Wrote this line: # The first word on each non comment line is the configuration option.
Wrote this line: # Remaining words or numbers on the line are configuration parameter
Wrote this line: # data fields.
Wrote this line:
Wrote this line: # Note that configuration option names are not case sensitive. However,
Wrote this line: # configuration parameter data is case sensitive and the lettercase must
Wrote this line: # be preserved.
Wrote this line:
Wrote this line: # This is a favourite fruit
Wrote this line: FAVOURITEFRUIT banana
Wrote this line:
Wrote this line: # This is a boolean that should be set
Wrote this line: ; NEEDSPEELING
Wrote this line:
Wrote this line: # This boolean is commented out
Wrote this line: SEEDSREMOVED
Wrote this line:
Wrote this line: # How many bananas we have
Wrote this line: NUMBEROFBANANAS 1024

```



## Fortran

Fortran has long had a built-in method for writing and reading a configuration file with ease, via the NAMELIST facility. The designers of the modern "configuration" files have paid not the slightest attention to the protocol, which is as follows:
```Fortran
      PROGRAM TEST	!Define some data aggregates, then write and read them.
      CHARACTER*28 FAVOURITEFRUIT
      LOGICAL NEEDSPEELING
      LOGICAL SEEDSREMOVED
      INTEGER NUMBEROFBANANAS
      NAMELIST /FRUIT/ FAVOURITEFRUIT,NEEDSPEELING,SEEDSREMOVED,
     1 NUMBEROFBANANAS
      INTEGER F	!An I/O unit number.
      F = 10	!This will do.

Create an example file to show its format.
      OPEN(F,FILE="Basket.txt",STATUS="REPLACE",ACTION="WRITE",	!First, prepare a recipient file.
     1 DELIM="QUOTE")	!CHARACTER variables will be enquoted.
      FAVOURITEFRUIT = "Banana"
      NEEDSPEELING = .TRUE.
      SEEDSREMOVED = .FALSE.
      NUMBEROFBANANAS = 48
      WRITE (F,FRUIT)		!Write the lot in one go.
      CLOSE (F)			!Finished with output.
Can now read from the file.
      OPEN(F,FILE="Basket.txt",STATUS="OLD",ACTION="READ",	!Get it back.
     1 DELIM="QUOTE")
      READ (F,FRUIT)			!Read who knows what.
      WRITE (6,FRUIT)
      END
```

Most of which is support stuff. The requirement is to declare a NAMELIST naming the variables of interest, then READ or WRITE using just the name of the NAMELIST. Only three statements, four if the file OPEN is counted.

''One statement to write them''

''One statement to read them''

''One statement to find them all, and in the NAMELIST bind them.''

(Apologies to J.R.R. Tolkien)

The result is a file in a standard layout.

```txt

 &FRUIT
 FAVOURITEFRUIT  = "Banana                      ",
 NEEDSPEELING    = T,
 SEEDSREMOVED    = F,
 NUMBEROFBANANAS =          48
 /

```

Evidently, the variables named in the NAMELIST are written out, one to a line in free-format with a style appropriate to its type, much as if they were assignment statements within a source file. Since Fortran compilers do not distinguish between upper and lower case, either may be used. The namelist's name starts the output, and the block of values ends with a line starting with a slash. On input, blocks will be skipped until the correct block name is found, so a single file may contain parameters for multiple usages. All output starts with column two so that column one can be used as a carriage control character if output is to go to a lineprinter. There is special provision for an array's value list whereby a run of equal values can be noted via <code>''n''*''value''</code> to save space, further, an input statement for an array can name a single element (the others being unaffected), and with F90, the usual array span facilities are available as in <code>A(6:10) = 5*3.14</code> Should an output list of values be too long for the standard line length (133, again to suit lineprinters, but this value can be changed by RECL = ''n'' in the OPEN statement) then the list will be split across lines after a comma. However, long CHARACTER variables will cross multiple lines and column one will be used, something of a mistake should such output go to a lineprinter as it may react badly to strange carriage control characters.

Input allows for a comma-separated list of assignments and the names may appear in any order. If a name appears more than once, each assignment will be accepted so that the last one counts. Names can be omitted - if so, the corresponding variable will be unaffected by a READ. It would be nice if, the variable having been declared in CamelStyle, the name would be printed that way, but alas. F90 introduces the ! character as an "escape" comment and this is recognised in the NAMELIST input, outside of text literals of course.

To remove the SeedsRemoved entry is straightforward, depending on context. If its name is removed from the NAMELIST then it will no longer be written. The parameter file could of course be edited before being read back in, perhaps by a human, or by the installation process of the new version, or by the new version first finding the unwanted line, backspacing the file one record and writing the right number of spaces to wipe it without changing the length of a record - though this ploy may not be available if the filesystem provides limited support. If however a new version of the programme is expected to read the old version's parameter file, then there will be trouble because an unknown name (or an array index out of bounds, but not a CHARACTER variable being shorter than the supplied text) will provoke an I/O error and the run will crash. This can be avoided via the ERR = ''label'' option in the READ statement, however entries after the erroneous one will not be processed.

In a more general situation, it is helpful to have a routine that reads the NAMELIST style input and isolates the assignments so that each can individually be written to a scratch file in the NAMELIST style (i.e. providing the block head and tail lines, though some Fortrans allow NAMELIST input from a text variable and without this requirement) whereupon if ERR is provoked during the READ, the troublesome entry can be displayed for user appreciation before continuing with any remaining assignments.

Otherwise, the old NAMELIST could be used for input, with the undesired value simply ignored within the new version of the programme. For output, a new NAMELIST would be devised omitting the unwanted name - the same variables can be named in more than one NAMELIST. However, every NAMELIST's name must be unique within a routine and this would be the new block header name. So, read the parameter file in one routine which declares the old NAMELIST, complete with the name of the now unwanted variable, but write it via another routine which declares the new NAMELIST using the same name but omitting the names of undesired variables. The wanted variables would be in COMMON, or with F90 onwards be common names in a MODULE, or one could mess with parameter lists.
```Fortran
      MODULE MONKEYFODDER
       INTEGER FIELD	!An I/O unit number.
       CHARACTER*28 FAVOURITEFRUIT
       LOGICAL NEEDSPEELING
       INTEGER NUMBEROFBANANAS
       CONTAINS
        SUBROUTINE GETVALS(FNAME)	!Reads values from some file.
         CHARACTER*(*) FNAME	!The file name.
         LOGICAL SEEDSREMOVED	!This variable is no longer wanted.
         NAMELIST /FRUIT/ FAVOURITEFRUIT,NEEDSPEELING,SEEDSREMOVED,	!But still appears in this list.
     1    NUMBEROFBANANAS
          OPEN(FIELD,FILE=FNAME,STATUS="OLD",ACTION="READ",	!Hopefully, the file exists.
     1     DELIM="QUOTE")	!Expect quoting for CHARACTER variables.
          READ (FIELD,FRUIT,ERR = 666)	!Read who knows what.
  666     CLOSE (FIELD)			!Ignoring any misformats.
        END SUBROUTINE GETVALS	!A proper routine would offer error messages.

        SUBROUTINE PUTVALS(FNAME)	!Writes values to some file.
         CHARACTER*(*) FNAME	!The file name.
         NAMELIST /FRUIT/ FAVOURITEFRUIT,NEEDSPEELING,NUMBEROFBANANAS
          OPEN(FIELD,FILE=FNAME,STATUS="REPLACE",ACTION="WRITE",	!Prepare a recipient file.
     1     DELIM="QUOTE")	!CHARACTER variables will be enquoted.
          WRITE (FIELD,FRUIT)	!Write however much is needed.
          CLOSE (FIELD)		!Finished for now.
        END SUBROUTINE PUTVALS
      END MODULE MONKEYFODDER

      PROGRAM TEST	!Updates the file created by an earlier version.
      USE MONKEYFODDER
      FIELD = 10	!This will do.
      CALL GETVALS("Basket.txt")	!Read the values, allowing for the previous version.
      CALL PUTVALS("Basket.txt")	!Save the values, as per the new version.
      END
```

Whereupon the file now has

```txt

 &FRUIT
 FAVOURITEFRUIT  = "Banana                      ",
 NEEDSPEELING    = T,
 NUMBEROFBANANAS =          48
 /

```


Naturally, a parameter file could contain data in whatever format was desired, and it might even employ a semicolon (of all things!) as a comment starter: such a file could be read and its components extracted via suitable code then written in the NAMELIST style to a scratch file and read back for the actual internalisation of values. The point of this is that a text name of a variable is associated with the actual computer variable via the NAMELIST facility, the programmer need not slog through some endless CASE statement on the names of the variables nor recognise the different data formats such as for complex numbers. This process would be reversed for output.

But given that the NAMELIST protocol is available without difficulty, why would a hard-core fortranner bother for a Fortran programme?


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type ConfigData
  favouriteFruit As String
  needsPeeling As Boolean
  seedsRemoved As Boolean
  numberOfBananas As UInteger
  numberOfStrawberries As UInteger
End Type

Sub updateConfigData(fileName As String, cData As ConfigData)
  Dim fileNum As Integer = FreeFile
  Open fileName For Input As #fileNum
  If err > 0 Then
    Print "File could not be opened"
    Sleep
    End
  End If
  Dim tempFileName As String = "temp_" + fileName
  Dim fileNum2 As Integer = FreeFile
  Open tempFileName For Output As #fileNum2
  Dim As Boolean hadFruit, hadPeeling, hadSeeds, hadBananas, hadStrawberries '' all false by default
  Dim As String ln
  While Not Eof(fileNum)
    Line Input #fileNum, ln
    If ln = "" OrElse Left(ln, 1) = "#" Then
      Print #fileNum2, ln
      Continue While
    End If
    ln = Trim(LTrim(ln, ";"), Any !" \t")
    If ln = "" Then Continue While
    If UCase(Left(ln, 14)) = "FAVOURITEFRUIT" Then
      If hadFruit Then Continue While
      hadFruit = True
      Print #fileNum2, "FAVOURITEFRUIT " + cData.favouriteFruit
    ElseIf UCase(Left(ln, 12)) = "NEEDSPEELING" Then
      If hadPeeling Then Continue While
      hadPeeling = True
      If cData.needsPeeling Then
        Print #fileNum2, "NEEDSPEELING"
      Else
        Print #fileNum2, "; NEEDSPEELING"
      End If
    ElseIf UCase(Left(ln, 12)) = "SEEDSREMOVED" Then
      If hadSeeds Then Continue While
      hadSeeds = True
      If cData.seedsRemoved Then
        Print #fileNum2, "SEEDSREMOVED"
      Else
        Print #fileNum2, "; SEEDSREMOVED"
      End If
    ElseIf UCase(Left(ln, 15)) = "NUMBEROFBANANAS" Then
      If hadBananas Then Continue While
      hadBananas = True
      Print #fileNum2, "NUMBEROFBANANAS " + Str(cData.numberOfBananas)
    ElseIf UCase(Left(ln, 20)) = "NUMBEROFSTRAWBERRIES" Then
      If hadStrawberries Then Continue While
      hadStrawberries = True
      Print #fileNum2, "NUMBEROFSTRAWBERRIES " + Str(cData.numberOfStrawBerries)
    End If
  Wend

  If Not hadFruit Then
     Print #fileNum2, "FAVOURITEFRUIT " + cData.favouriteFruit
  End If
  If Not hadPeeling Then
    If cData.needsPeeling Then
      Print #fileNum2, "NEEDSPEELING"
    Else
      Print #fileNum2, "; NEEDSPEELING"
    End If
  End If
  If Not hadSeeds Then
    If cData.seedsRemoved Then
      Print #fileNum2, "SEEDSREMOVED"
    Else
      Print #fileNum2, "; SEEDSREMOVED"
    End If
  End If
  If Not hadBananas Then
    Print #fileNum2, "NUMBEROFBANANAS " + Str(cData.numberOfBananas)
  End If
  If Not hadStrawberries Then
    Print #fileNum2, "NUMBEROFSTRAWBERRIES " + Str(cData.numberOfStrawBerries)
  End If

  Close #fileNum : Close #fileNum2
  Kill(fileName)
  Name (tempFileName fileName)
End Sub

Dim fileName As String = "config2.txt"
Dim cData As ConfigData
With cData
  .favouriteFruit = "banana"
  .needsPeeling = False
  .seedsRemoved = True
  .numberOfBananas = 1024
  .numberOfStrawberries = 62000
End With

updateConfigData fileName, cData
Print
Print "Press any key to quit"
Sleep
```


The contents of config2.txt after updating are:
```txt

# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000

```



## Go


```go
package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"unicode"
)

// line represents a single line in the configuration file.
type line struct {
	kind     lineKind
	option   string
	value    string
	disabled bool
}

// lineKind represents the different kinds of configuration line.
type lineKind int

const (
	_ lineKind = iota
	ignore
	parseError
	comment
	blank
	value
)

func (l line) String() string {
	switch l.kind {
	case ignore, parseError, comment, blank:
		return l.value
	case value:
		s := l.option
		if l.disabled {
			s = "; " + s
		}
		if l.value != "" {
			s += " " + l.value
		}
		return s
	}
	panic("unexpected line kind")
}

func removeDross(s string) string {
	return strings.Map(func(r rune) rune {
		if r < 32 || r > 0x7f || unicode.IsControl(r) {
			return -1
		}
		return r
	}, s)
}

func parseLine(s string) line {
	if s == "" {
		return line{kind: blank}
	}
	if s[0] == '#' {
		return line{kind: comment, value: s}
	}
	s = removeDross(s)
	fields := strings.Fields(s)
	if len(fields) == 0 {
		return line{kind: blank}
	}
	// Strip leading semicolons (but record that we found them)
	semi := false
	for len(fields[0]) > 0 && fields[0][0] == ';' {
		semi = true
		fields[0] = fields[0][1:]
	}
	// Lose the first field if it was all semicolons
	if fields[0] == "" {
		fields = fields[1:]
	}
	switch len(fields) {
	case 0:
		// This can only happen if the line starts
		// with a semicolon but has no other information
		return line{kind: ignore}
	case 1:
		return line{
			kind:     value,
			option:   strings.ToUpper(fields[0]),
			disabled: semi,
		}
	case 2:
		return line{
			kind:     value,
			option:   strings.ToUpper(fields[0]),
			value:    fields[1],
			disabled: semi,
		}
	}
	return line{kind: parseError, value: s}
}

// Config represents a "standard" configuration file.
type Config struct {
	options map[string]int		// index of each option in lines.
	lines   []line
}

// index returns the index of the given option in
// c.lines, or -1 if not found.
func (c *Config) index(option string) int {
	if i, ok := c.options[option]; ok {
		return i
	}
	return -1
}

// addLine adds a line to the config, ignoring
// duplicate options and "ignore" lines.
func (c *Config) addLine(l line) {
	switch l.kind {
	case ignore:
		return
	case value:
		if c.index(l.option) >= 0 {
			return
		}
		c.options[l.option] = len(c.lines)
		c.lines = append(c.lines, l)
	default:
		c.lines = append(c.lines, l)
	}
}

// ReadConfig reads a configuration file from path and returns it.
func ReadConfig(path string) (*Config, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	r := bufio.NewReader(f)
	c := &Config{options: make(map[string]int)}
	for {
		s, err := r.ReadString('\n')
		if s != "" {
			if err == nil {
				// strip newline unless we encountered an error without finding one.
				s = s[:len(s)-1]
			}
			c.addLine(parseLine(s))
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
	}
	return c, nil
}

// Set sets an option to a value, adding the option if necessary.  If
// the option was previously disabled, it will be enabled.
func (c *Config) Set(option string, val string) {
	if i := c.index(option); i >= 0 {
		line := &c.lines[i]
		line.disabled = false
		line.value = val
		return
	}
	c.addLine(line{
		kind:   value,
		option: option,
		value:  val,
	})
}

// Enable sets the enabled status of an option. It is
// ignored if the option does not already exist.
func (c *Config) Enable(option string, enabled bool) {
	if i := c.index(option); i >= 0 {
		c.lines[i].disabled = !enabled
	}
}

// Write writes the configuration file to the writer.
func (c *Config) Write(w io.Writer) {
	for _, line := range c.lines {
		fmt.Println(line)
	}
}

func main() {
	c, err := ReadConfig("/tmp/cfg")
	if err != nil {
		log.Fatalln(err)
	}
	c.Enable("NEEDSPEELING", false)
	c.Set("SEEDSREMOVED", "")
	c.Set("NUMBEROFBANANAS", "1024")
	c.Set("NUMBEROFSTRAWBERRIES", "62000")
	c.Write(os.Stdout)
}
```



## Haskell


Necessary imports


```Haskell
import Data.Char (toUpper)
import qualified System.IO.Strict  as S
```


Definition of datatypes:


```Haskell
data INI = INI { entries :: [Entry] } deriving Show

data Entry = Comment String
           | Field String String
           | Flag String Bool
           | EmptyLine

instance Show Entry where
  show entry = case entry of
    Comment text -> "# " ++ text
    Field f v    -> f ++ " " ++ v
    Flag f True  -> f
    Flag f False -> "; " ++ f
    EmptyLine    -> ""

instance Read Entry where
  readsPrec _ s = [(interprete (clean " " s), "")]
    where
      clean chs = dropWhile (`elem` chs)
      interprete ('#' : text) = Comment text
      interprete (';' : f)= flag (clean " ;" f) False
      interprete entry = case words entry of
        []    -> EmptyLine
        [f]   -> flag f True
        f : v -> field f (unwords v)
      field f = Field (toUpper <$> f)
      flag f = Flag (toUpper <$> f)
```


Getting and setting fields in INI data:


```Haskell
setValue :: String -> String -> INI -> INI
setValue f v = INI . replaceOn (eqv f) (Field f v) . entries

setFlag :: String -> Bool -> INI -> INI
setFlag f v = INI . replaceOn (eqv f) (Flag f v) . entries

enable f = setFlag f True
disable f = setFlag f False

eqv f entry = (toUpper <$> f) == (toUpper <$> field entry)
  where field (Field f _) = f
        field (Flag f _) = f
        field _ = ""

replaceOn p x lst = prev ++ x : post
  where
    (prev,post) = case break p lst of
      (lst, []) -> (lst, [])
      (lst, _:xs) -> (lst, xs)
```


IO stuff:


```Haskell
readIni :: String -> IO INI
readIni file = INI . map read . lines <$> S.readFile file

writeIni :: String -> INI -> IO ()
writeIni file = writeFile file . unlines . map show . entries

updateIni :: String -> (INI -> INI) -> IO ()
updateIni file f = readIni file >>= writeIni file . f

main = updateIni "test.ini" $
  disable "NeedsPeeling" .
  enable "SeedsRemoved" .
  setValue "NumberOfBananas" "1024" .
  setValue "NumberOfStrawberries" "62000"
```



## J


Since the task does not specify the line end character, we assume that the last character in the file is the line end character.


```J
require 'regex strings'

normalize=:3 :0
  seen=. a:
  eol=. {:;y
  t=. ''
  for_line.<;._2;y do. lin=. deb line=.>line
    if. '#'={.line do. t=.t,line,eol
    elseif. ''-:lin do. t =. t,eol
    elseif. do.
      line=. 1 u:([-.-.)&(32+i.95)&.(3&u:) line
      base=. ('^ *;;* *';'') rxrplc line
      nm=. ;name=. {.;:toupper base
      if. -. name e. seen do.
        seen=. seen, name
        t=. t,eol,~dtb ('; '#~';'={.lin),(('^(?i) *',nm,'\b *');(nm,' ')) rxrplc base
      end.
    end.
  end.t
)

enable=:1 :0
  (<m) 1!:2~ normalize (,y,{:);<@rxrplc~&(y;~'^; *(?i)',y,'\b');.2]1!:1<m
)

disable=:1 :0
  (<m) 1!:2~ normalize (,'; ',y,{:);<@rxrplc~&(('; ',y);~'^ *(?i)',y,'\b');.2]1!:1<m
)

set=:1 :0
:
  t=. 1!:1<m
  pat=. '^ *(?i)',y,'\b.*'
  upd=. y,' ',":x
  (<m) 1!:2~ normalize (,upd,{:);<@rxrplc~&(pat;upd) t
)
```


Note that, aside from the line end issue, the task is ambiguous because it specifies a set of operations rather than a file format.  If the consequences of these ambiguities are troubling, you might prefer to replace normalize with normalize^:_

Example use:


```J
   'example.file' disable 'needspeeling'
   'example.file' enable 'seedsremoved'
   1024 'example.file' set 'numberofbananas'
   62000 'example.file' set 'numberofstrawberries'
```


Here's how the file specified in the task description looks after these steps have been executed:

<pre style="height:25ex;overflow:scroll"># This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024

NUMBEROFSTRAWBERRIES 62000

```



## Java

```java
import java.io.*;
import java.util.*;
import java.util.regex.*;

public class UpdateConfig {

    public static void main(String[] args) {
        if (args[0] == null) {
            System.out.println("filename required");

        } else if (readConfig(args[0])) {
            enableOption("seedsremoved");
            disableOption("needspeeling");
            setOption("numberofbananas", "1024");
            addOption("numberofstrawberries", "62000");
            store();
        }
    }

    private enum EntryType {
        EMPTY, ENABLED, DISABLED, COMMENT
    }

    private static class Entry {
        EntryType type;
        String name, value;

        Entry(EntryType t, String n, String v) {
            type = t;
            name = n;
            value = v;
        }
    }

    private static Map<String, Entry> entries = new LinkedHashMap<>();
    private static String path;

    private static boolean readConfig(String p) {
        path = p;

        File f = new File(path);
        if (!f.exists() || f.isDirectory())
            return false;

        String regexString = "^(;*)\\s*([A-Za-z0-9]+)\\s*([A-Za-z0-9]*)";
        Pattern regex = Pattern.compile(regexString);

        try (Scanner sc = new Scanner(new FileReader(f))){
            int emptyLines = 0;
            String line;
            while (sc.hasNext()) {
                line = sc.nextLine().trim();

                if (line.isEmpty()) {
                    addOption("" + emptyLines++, null, EntryType.EMPTY);

                } else if (line.charAt(0) == '#') {
                    entries.put(line, new Entry(EntryType.COMMENT, line, null));

                } else {
                    line = line.replaceAll("[^a-zA-Z0-9\\x20;]", "");
                    Matcher m = regex.matcher(line);

                    if (m.find() && !m.group(2).isEmpty()) {

                        EntryType t = EntryType.ENABLED;
                        if (!m.group(1).isEmpty())
                            t = EntryType.DISABLED;

                        addOption(m.group(2), m.group(3), t);
                    }
                }
            }
        } catch (IOException e) {
            System.out.println(e);
        }
        return true;
    }

    private static void addOption(String name, String value) {
        addOption(name, value, EntryType.ENABLED);
    }

    private static void addOption(String name, String value, EntryType t) {
        name = name.toUpperCase();
        entries.put(name, new Entry(t, name, value));
    }

    private static void enableOption(String name) {
        Entry e = entries.get(name.toUpperCase());
        if (e != null)
            e.type = EntryType.ENABLED;
    }

    private static void disableOption(String name) {
        Entry e = entries.get(name.toUpperCase());
        if (e != null)
            e.type = EntryType.DISABLED;
    }

    private static void setOption(String name, String value) {
        Entry e = entries.get(name.toUpperCase());
        if (e != null)
            e.value = value;
    }

    private static void store() {
        try (PrintWriter pw = new PrintWriter(path)) {
            for (Entry e : entries.values()) {
                switch (e.type) {
                    case EMPTY:
                        pw.println();
                        break;
                    case ENABLED:
                        pw.format("%s %s%n", e.name, e.value);
                        break;
                    case DISABLED:
                        pw.format("; %s %s%n", e.name, e.value);
                        break;
                    case COMMENT:
                        pw.println(e.name);
                        break;
                    default:
                        break;
                }
            }
            if (pw.checkError()) {
                throw new IOException("writing to file failed");
            }
        } catch (IOException e) {
            System.out.println(e);
        }
    }
}
```


Input file:


```txt
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
    # be preserved.

# This is a favourite fruit
FAVOURITEFRUIT    ßßßßß		banana    µµµµ

# This is a boolean that should be set
NEEDspeeling

# This boolean is commented out
;;;; SEEDSREMOVED µµµµµ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

# How many bananas we have
    NUMBEROFBANANAS µµµµµ 48
```


Output:


```txt
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000
```




## Julia

Designed similarly to the way multiple small functions in a Julia module for general editing of this file type might be written.

```julia
function cleansyntax(line)
    line = strip(line)
    if line == ""
        return "#=blank line=#"
    elseif line[1] == '#'
        return line
    elseif line[1] == ';'
        line = replace(line, r"^;[;]+" => ";")
    else # active option
        o, p = splitline(line)
        line = p == nothing ? uppercase(o) : uppercase(o) * " " * p
    end
    join(filter(c -> isascii(c[1]) && !iscntrl(c[1]), split(line, "")), "")
end

"""
    Run to clean up the configuration file lines prior to other function application.
"""
cleansyntax!(lines) = (for (i, li) in enumerate(lines) lines[i] = cleansyntax(li) end; lines)

isdisabled(line) = startswith(line, [';', '#'])
isenabled(line) = !isdisabled(line)

function splitline(line)
    arr = split(line, r"\s+", limit=2)
    if length(arr) < 2
        return (arr[1], nothing)
    end
    return (arr[1], arr[2])
end

function disable!(lines, opt)
    for (i, li) in enumerate(lines)
        if isenabled(li) && splitline(li)[1] == uppercase(opt)
            lines[i] = ";" * li
            break # note: only first one found is disabled
        end
    end
    lines
end

function enable!(lines, opt)
    for (i, li) in enumerate(lines)
        if isdisabled(li)
            s = li[2:end]
            if splitline(s)[1] == uppercase(opt)
                lines[i] = s
                break # note: only first one found is enabled
            end
        end
    end
    lines
end

function changeparam!(lines, opt, newparam)
    for (i, li) in enumerate(lines)
        if isenabled(li)
            o, p = splitline(li)
            if o == opt
                lines[i] = o * " " * string(newparam)
                break # note: only first one found is changed
            end
        end
    end
    lines
end

function activecfg(lines)
    cfgdict = Dict()
    for li in lines
        if isenabled(li)
            o, p = splitline(li)
            cfgdict[o] = p
        end
    end
    cfgdict
end

const filename = "fruit.cfg"
const cfh = open(filename)
const cfglines = cleansyntax!(readlines(cfh))
close(cfh)

const cfg = activecfg(cfglines)

disable!(cfglines, "NEEDSPEELING")
enable!(cfglines, "SEEDSREMOVED")
changeparam!(cfglines, "NUMBEROFBANANAS", 1024)

if !haskey(cfg, "NUMBEROFSTRAWBERRIES")
    push!(cfglines, "NUMBEROFSTRAWBERRIES 62000")
end
cfg["NUMBEROFSTRAWBERRIES"] = 62000
changeparam!(cfglines, "NUMBEROFSTRAWBERRIES", 62000)

const cfw = open(filename, "w")
for li in cfglines
    if li != ""
        if li == "#=blank line=#"
            li = ""
        end
        write(cfw, li * "\n")
    end
end

```
 {{output}}
```txt

 Contents of the revised file:
 # This is a configuration file in standard configuration file format
 #
 # Lines begininning with a hash or a semicolon are ignored by the application
 # program. Blank lines are also ignored by the application program.

 # The first word on each non comment line is the configuration option.
 # Remaining words or numbers on the line are configuration parameter
 # data fields.

 # Note that configuration option names are not case sensitive. However,
 # configuration parameter data is case sensitive and the lettercase must
 # be preserved.

 # This is a favourite fruit
 FAVOURITEFRUIT banana

 # This is a boolean that should be set
 ;NEEDSPEELING

 # This boolean is commented out
 ; SEEDSREMOVED

 # How many bananas we have
 NUMBEROFBANANAS 1024
 NUMBEROFSTRAWBERRIES 62000

```



## Kotlin

```scala
// version 1.2.0

import java.io.File

class ConfigData(
    val favouriteFruit: String,
    val needsPeeling: Boolean,
    val seedsRemoved: Boolean,
    val numberOfBananas: Int,
    val numberOfStrawberries: Int
)

fun updateConfigFile(fileName: String, cData: ConfigData) {
    val inp = File(fileName)
    val lines = inp.readLines()
    val tempFileName = "temp_$fileName"
    val out = File(tempFileName)
    val pw = out.printWriter()
    var hadFruit = false
    var hadPeeling = false
    var hadSeeds = false
    var hadBananas = false
    var hadStrawberries = false

    for (line in lines) {
        if (line.isEmpty() || line[0] == '#') {
            pw.println(line)
            continue
        }
        val ln = line.trimStart(';').trim(' ', '\t').toUpperCase()
        if (ln.isEmpty()) continue
        if (ln.take(14) == "FAVOURITEFRUIT") {
            if (hadFruit) continue
            hadFruit = true
            pw.println("FAVOURITEFRUIT ${cData.favouriteFruit}")
        }
        else if (ln.take(12) == "NEEDSPEELING") {
            if (hadPeeling) continue
            hadPeeling = true
            if (cData.needsPeeling)
                pw.println("NEEDSPEELING")
            else
                pw.println("; NEEDSPEELING")
        }
        else if (ln.take(12) == "SEEDSREMOVED") {
            if (hadSeeds) continue
            hadSeeds = true
            if (cData.seedsRemoved)
                pw.println("SEEDSREMOVED")
            else
                pw.println("; SEEDSREMOVED")
        }
        else if(ln.take(15) == "NUMBEROFBANANAS") {
            if (hadBananas) continue
            hadBananas = true
            pw.println("NUMBEROFBANANAS ${cData.numberOfBananas}")
        }
        else if(ln.take(20) == "NUMBEROFSTRAWBERRIES") {
            if (hadStrawberries) continue
            hadStrawberries = true
            pw.println("NUMBEROFSTRAWBERRIES ${cData.numberOfStrawberries}")
        }
    }

    if (!hadFruit) {
        pw.println("FAVOURITEFRUIT ${cData.favouriteFruit}")
    }

    if (!hadPeeling) {
        if (cData.needsPeeling)
            pw.println("NEEDSPEELING")
        else
            pw.println("; NEEDSPEELING")
    }

    if (!hadSeeds) {
        if (cData.seedsRemoved)
            pw.println("SEEDSREMOVED")
        else
            pw.println("; SEEDSREMOVED")
    }

    if (!hadBananas) {
       pw.println("NUMBEROFBANANAS ${cData.numberOfBananas}")
    }

    if (!hadStrawberries) {
       pw.println("NUMBEROFSTRAWBERRIES ${cData.numberOfStrawberries}")
    }

    pw.close()
    inp.delete()
    out.renameTo(inp)
}

fun main(args: Array<String>) {
    val fileName = "config.txt"
    val cData = ConfigData("banana", false, true, 1024, 62000)
    updateConfigFile(fileName, cData)
}
```


Contents of file 'config.txt' after updating:

```txt

# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000

```



## Lasso

Config type definition

```Lasso
#!/usr/bin/lasso9

define config => type {

	data public configtxt, public path

	public oncreate(
		path::string = 'testing/configuration.txt'
	) => {
		.configtxt = file(#path) -> readstring
		.path = #path
	}

	public get(term::string) => {
		.clean
		local(
			regexp	= regexp(-find = `(?m)^` + #term + `($|\s*=\s*|\s+)(.*)$`, -input = .configtxt, -ignorecase),
			result
		)

		while(#regexp -> find) => {
			#result = (#regexp -> groupcount > 1 ? (#regexp -> matchString(2) -> trim& || true))
			if(#result -> asstring >> ',') => {
				#result = #result -> split(',')
				#result -> foreach => {#1 -> trim}
			}
			return #result
		}
		return false
	}

	public set(term::string, value) => {
		if(#value === false) => {
			.disable(#term)
			return
		}
		.enable(#term)
		if(#value -> isanyof(::string, ::integer, ::decimal)) => {
			.configtxt = regexp(-find = `(?m)^(` + #term + `) ?(.*?)$`, -replace = `$1 ` + #value, -input = .configtxt, -ignorecase) -> replaceall
		}
	}

	public disable(term::string) => {
		.clean
		local(regexp = regexp(-find = `(?m)^(` + #term + `)`, -replace = `; $1`, -input = .configtxt, -ignorecase))
		.configtxt = #regexp -> replaceall
	}

	public enable(term::string, -comment::string = '# Added ' + date) => {
		.clean
		local(regexp = regexp(-find = `(?m)^(; )?(` + #term + `)`, -replace = `$2`, -input = .configtxt, -ignorecase))
		if(#regexp -> find) => {
			.configtxt = #regexp -> replaceall
		else
			.configtxt -> append('\n' + (not #comment -> beginswith('#') ? '# ') +
				#comment + '\n' +
				string_uppercase(#term) + '\n'
			)
		}
	}

	public write => {
		local(config = file(.path))
		#config -> opentruncate
		#config -> dowithclose => {
			#config -> writestring(.configtxt)
		}

	}

	public clean => {

		local(
			cleaned = array,
			regexp	= regexp(-find = `^(;+)\W*$`)
		)

		with line in .configtxt -> split('\n') do {
			#line -> trim
			#regexp -> input = #line

			if(#line -> beginswith('#') or #line == '') => {
				#cleaned -> insert(#line)
			else(not (#regexp -> find))
				if(#line -> beginswith(';')) => {
					#line -> replace(regexp(`^;+ *`), `; `)
				else
					#line -> replace(regexp(`^(.*?) +(.*?)`), `$1 $2`)
				}
				#line -> replace(regexp(`\t`))
				#cleaned -> insert(#line)
			}
		}

		.configtxt = #cleaned -> join('\n')

	}

}
```


How to call it:

```Lasso

local(
	config		= config,
)

stdoutnl(#config -> get('FAVOURITEFRUIT'))
stdoutnl(#config -> get('SEEDSREMOVED'))
stdoutnl(#config -> get('NUMBEROFBANANAS'))

#config -> enable('seedsremoved')
#config -> enable('PARAMWITHCOMMENT', -comment = 'This param was added to demonstrate the possibility to also have comments associated with it')
#config -> disable('needspeeling')
#config -> set('numberofstrawberries', 62000)
#config -> set('numberofbananas', 1024)

#config -> write
```


Initial config file:

```txt
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

 # This is a favourite fruit
FAVOURITEFRUIT   banana

# This is a boolean that should be set
 NEEDSPEELING

# This boolean is commented out
 ;  SEEDSREMOVED

# This boolean is commented out
 ;  HEEDSREMOVED

# Another option
;; OOPS Remove 	the double ;
;
;;

# How many bananas we have
NUMBEROFBANANAS 48

```

Final config file:

```txt
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# This boolean is commented out
; HEEDSREMOVED

# Another option
; OOPS Remove the double ;

# How many bananas we have
NUMBEROFBANANAS 1024

# This param was added to demonstrate the possibility to also have comments associated with it
PARAMWITHCOMMENT

# Added 2013-12-01 00:32:00
NUMBEROFSTRAWBERRIES 62000

```



## Perl



```Perl
use warnings;
use strict;

my $needspeeling         = 0;
my $seedsremoved         = 1;
my $numberofstrawberries = 62000;
my $numberofbananas      = 1024;
my $favouritefruit       = 'bananas';

my @out;

sub config {
    my (@config) = <DATA>;
    push @config, "NUMBEROFSTRAWBERRIES $numberofstrawberries\n"
        unless grep { /^;*[ \t]*NUMBEROFSTRAWBERRIES\b/; } @config;

    foreach my $line (@config) {

        if (substr($line, 0, 1) eq '#') {
            push @out, $line;
            next;
        }

        next if $line =~ /^[;\t ]+$/;

        my ($option, $option_name);

        if ($line =~ /^([A-Z]+[0-9]*)/) {
            $option_name = lc $1;
            $option      = eval "\\\$$option_name";
            my $value = eval "\${$option_name}";

            if ($value) {
                $$option = $value;
            }
            else {
                $line    = '; ' . $line;
                $$option = undef;
            }
        }
        elsif ($line =~ /^;+\s*([A-Z]+[0-9]*)/) {
            $option_name = lc $1;
            $option      = eval "\\\$$option_name";
            my $value = eval "\${$option_name}";

            if ($value) {
                $line =~ s/^;+\s*//;
                $$option = $value == 1 ? '' : $value;
            }
            else {
                $$option = undef;
            }
        }

        if (defined $$option) {
            push @out, "\U$option_name\E $$option\n"
                unless grep { /^\U$option_name\E\b/; } @out;
        }
        else {
            $line =~ s/\s*[^[:ascii:]]+\s*$//;
            $line =~ s/[[:cntrl:]\s]+$//;
            push(@out, "$line\n");
        }
    }
}

config();

my $file = join('', @out);
$file =~ s/\n{3,}/\n\n/g;

print $file;

__DATA__
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
NEEDSPEELING

# This boolean is commented out
; SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 48
```



## Perl 6

Implemented as a command-line script which can make arbitrary in-place updates to such config files.

Assuming that the script is saved as <tt>conf-update</tt> and the config file as <tt>test.cfg</tt>, the four changes required by the task description could be performed with the command:


```txt
conf-update --/needspeeling --seedsremoved --numberofbananas=1024 --numberofstrawberries=62000 test.cfg
```


The script:


```perl6
use File::Temp;

my ($tmpfile, $out) = tempfile;

sub MAIN ($file, *%changes) {
    %changes.=map({; .key.uc => .value });
    my %seen;

    for $file.IO.lines {
        when /:s ^ ('#' .* | '') $/ {
            say $out: ~$0;
        }
        when /:s ^ (';'+)? [(\w+) (\w+)?]? $/ {
            next if !$1 or %seen{$1.uc}++;
            my $new = %changes{$1.uc}:delete;
            say $out: format-line $1, |( !defined($new)  ?? ($2, !$0)  !!
                                         $new ~~ Bool    ?? ($2, $new) !! ($new, True) );
        }
        default {
            note "Malformed line: $_\nAborting.";
            exit 1;
        }
    }

    say $out: format-line .key, |(.value ~~ Bool ?? (Nil, .value) !! (.value, True))
        for %changes;

    $out.close;

    copy $tmpfile, $file;
}

sub format-line ($key, $value, $enabled) {
    ("; " if !$enabled) ~ $key.uc ~ (" $value" if defined $value);
}
```



## Phix

Very basic (and contains most of the code from the read configuration file example)

Note in particular there is no real attempt to distinguish between booleans and integers.

```Phix
integer fn = open("RCTEST.INI","r")
sequence lines = get_text(fn,GT_LF_STRIPPED)
close(fn)
constant dini = new_dict()
for i=1 to length(lines) do
    string li = trim(lines[i])
    if length(li)
    and not find(li[1],"#;") then
        integer k = find(' ',li)
        if k!=0 then
            string rest = li[k+1..$]
            li = upper(li[1..k-1])
            putd(li,rest,dini)
        else
            putd(upper(li),1,dini)
        end if
    end if
end for

deld("NEEDSPEELING",dini)
setd("SEEDSREMOVED",1,dini)
setd("NUMBEROFBANANAS",1024,dini)
setd("NUMBEROFSTRAWBERRIES",62000,dini)

for i=1 to length(lines) do
    string li = trim(lines[i])
    if length(li)
    and li[1]!='#' then
        if li[1]=';' then
            li = trim(li[2..$])
        end if
        integer k = find(' ',li)
        if k!=0 then
            string rest = li[k+1..$]
            li = upper(li[1..k-1])
            k = getd_index(li,dini)
            if k=0 then
                lines[i] = "; "&li&" "&rest
            else
                object o = getd_by_index(k,dini)
                if not string(o) then o = sprint(o) end if
                lines[i] = li&" "&o
                deld(li,dini)
            end if
        else
            if getd(li,dini) then
                lines[i] = li
                deld(li,dini)
            else
                lines[i] = "; "&li
            end if
        end if
    end if
end for
function visitor(object key, object data, object /*user_data*/)
    lines = append(lines,key&" "&sprint(data))
    return 1
end function
traverse_dict(routine_id("visitor"),0,dini)
fn = open("RCTEST.INI","w")
puts(fn,join(lines,"\n"))
close(fn)
```

Resulting RCTEST.INI file:

```txt

# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000

```



## PicoLisp


```PicoLisp
(let Data  # Read all data
   (in "config"
      (make
         (until (eof)
            (link (trim (split (line) " "))) ) ) )
   (setq Data  # Fix comments
      (mapcar
         '((L)
            (while (head '(";" ";") (car L))
               (pop L) )
            (if (= '(";") (car L))
               L
               (cons NIL L) ) )
         Data ) )
   (let (Need NIL  Seed NIL  NBan NIL  NStr NIL  Favo NIL)
      (map
         '((L)
            (let D (mapcar uppc (cadar L))
               (cond
                  ((= '`(chop "NEEDSPEELING") D)
                     (if Need
                        (set L)
                        (on Need)
                        (unless (caar L)
                           (set (car L) '(";")) ) ) )
                  ((= '`(chop "SEEDSREMOVED") D)
                     (if Seed
                        (set L)
                        (on Seed)
                        (when (caar L)
                           (set (car L)) ) ) )
                  ((= '`(chop "NUMBEROFBANANAS") D)
                     (if NBan
                        (set L)
                        (on NBan)
                        (set (cddar L) 1024) ) )
                  ((= '`(chop "NUMBEROFSTRAWBERRIES") D)
                     (if NStr
                        (set L)
                        (on NStr) ) )
                  ((= '`(chop "FAVOURITEFRUIT") D)
                     (if Favo
                        (set L)
                        (on Favo) ) ) ) ) )
         Data )
      (unless Need
         (conc Data (cons (list NIL "NEEDSPEELING"))) )
      (unless Seed
         (conc Data (cons (list NIL "SEEDSREMOVED"))) )
      (unless NBan
         (conc Data (cons (list NIL "NUMBEROFBANANAS" 1024))) )
      (unless NStr
         (conc Data (cons (list NIL "NUMBEROFSTRAWBERRIES" 62000))) ) )
   (out "config"
      (for L Data
         (prinl (glue " " (if (car L) L (cdr L)))) ) ) )
```



## PowerShell


```PowerShell

function Update-ConfigurationFile
{
    [CmdletBinding()]
    Param
    (
        [Parameter(Mandatory=$false,
                   Position=0)]
        [ValidateScript({Test-Path $_})]
        [string]
        $Path = ".\config.txt",

        [Parameter(Mandatory=$false)]
        [string]
        $FavouriteFruit,

        [Parameter(Mandatory=$false)]
        [int]
        $NumberOfBananas,

        [Parameter(Mandatory=$false)]
        [int]
        $NumberOfStrawberries,

        [Parameter(Mandatory=$false)]
        [ValidateSet("On", "Off")]
        [string]
        $NeedsPeeling,

        [Parameter(Mandatory=$false)]
        [ValidateSet("On", "Off")]
        [string]
        $SeedsRemoved
    )

    [string[]]$lines = Get-Content $Path

    Clear-Content $Path

    if (-not ($lines | Select-String -Pattern "^\s*NumberOfStrawberries" -Quiet))
    {
        "", "# How many strawberries we have", "NumberOfStrawberries 0" | ForEach-Object {$lines += $_}
    }

    foreach ($line in $lines)
    {
        $line = $line -replace "^\s*",""  ## Strip leading whitespace

        if ($line -match "[;].*\s*") {continue}  ## Strip semicolons

        switch -Regex ($line)
        {
            "(^$)|(^#\s.*)"                                              ## Blank line or comment
            {
                $line = $line
            }
            "^FavouriteFruit\s*.*"                                       ## Parameter FavouriteFruit
            {
                if ($FavouriteFruit)
                {
                    $line = "FAVOURITEFRUIT $FavouriteFruit"
                }
            }
            "^NumberOfBananas\s*.*"                                      ## Parameter NumberOfBananas
            {
                if ($NumberOfBananas)
                {
                    $line = "NUMBEROFBANANAS $NumberOfBananas"
                }
            }
            "^NumberOfStrawberries\s*.*"                                 ## Parameter NumberOfStrawberries
            {
                if ($NumberOfStrawberries)
                {
                    $line = "NUMBEROFSTRAWBERRIES $NumberOfStrawberries"
                }
            }
            ".*NeedsPeeling\s*.*"                                        ## Parameter NeedsPeeling
            {
                if ($NeedsPeeling -eq "On")
                {
                    $line = "NEEDSPEELING"
                }
                elseif ($NeedsPeeling -eq "Off")
                {
                    $line = "; NEEDSPEELING"
                }
            }
            ".*SeedsRemoved\s*.*"                                        ## Parameter SeedsRemoved
            {
                if ($SeedsRemoved -eq "On")
                {
                    $line = "SEEDSREMOVED"
                }
                elseif ($SeedsRemoved -eq "Off")
                {
                    $line = "; SEEDSREMOVED"
                }
            }
            Default                                                      ## Whatever...
            {
                $line = $line
            }
        }

        Add-Content $Path -Value $line -Force
    }
}

```


```PowerShell

Update-ConfigurationFile -NumberOfStrawberries 62000 -NumberOfBananas 1024 -SeedsRemoved On -NeedsPeeling Off

Invoke-Item -Path ".\config.txt"

```

```txt

# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024

# How many strawberries we have
NUMBEROFSTRAWBERRIES 62000

```



## Python


```Python
#!/usr/bin/env python

#----------------------------------------------------------------------------
# STANDARD MODULES
#----------------------------------------------------------------------------
import re
import string


#----------------------------------------------------------------------------
# GLOBAL: VARIABLES
#----------------------------------------------------------------------------
DISABLED_PREFIX = ';'


#----------------------------------------------------------------------------
# CLASS Option
#----------------------------------------------------------------------------
class Option(object):
    """An option, characterized by its name and its (optional) value. and by
       its status, which can be enabled or disabled.
       If its value is None, it is regarded to as a boolean option with a
       value of true.
    """

    #------------------------------------------------------------------------
    def __init__(self, name, value=None, disabled=False,
                 disabled_prefix=DISABLED_PREFIX):
        """Create an Option instance, setting its name to 'name' (always
           converted to a string) and its value to 'value'. If 'disabled' is
           True, the option is considered disabled, otherwise enabled.
           The string 'disabled_prefix' is used as a prefix when generating the
           string representation of the option.
        """
        self.name = str(name)
        self.value = value
        self.disabled = bool(disabled)
        self.disabled_prefix = disabled_prefix

    #------------------------------------------------------------------------
    def __str__(self):
        """Return a string representation of the Option instance.
           This always includes the option name, followed by a space and the
           option value (if it is not None). If the option is disabled, the
           whole string is preprendend by the string stored in the instance
           attribute 'disabled_prefix' and a space.
        """
        disabled = ('', '%s ' % self.disabled_prefix)[self.disabled]
        value = (' %s' % self.value, '')[self.value is None]
        return ''.join((disabled, self.name, value))

    #------------------------------------------------------------------------
    def get(self):
        """Return the option value.
           If the stored value is None, the option is regarded to as a
           boolean one and its enabled status is returned. Othrwise its value
           is returned.
        """
        enabled = not bool(self.disabled)
        if self.value is None:
            value = enabled
        else:
            value = enabled and self.value
        return value


#----------------------------------------------------------------------------
# CLASS Config
#----------------------------------------------------------------------------
class Config(object):
    """A set of configuration options and comment strings.
    """
    # Regular expression matching a valid option line.
    reOPTION = r'^\s*(?P<disabled>%s*)\s*(?P<name>\w+)(?:\s+(?P<value>.+?))?\s*$'

    #------------------------------------------------------------------------
    def __init__(self, fname=None, disabled_prefix=DISABLED_PREFIX):
        """Initialize a Config instance, optionally reading the contents of
           the configuration file 'fname'.
           The string 'disabled_prefix' is used as a prefix when generating the
           string representation of the options.
        """
        self.disabled_prefix = disabled_prefix
        self.contents = []          # Sequence of strings and Option instances.
        self.options = {}           # Map an option name to an Option instance.
        self.creOPTION = re.compile(self.reOPTION % self.disabled_prefix)
        if fname:
            self.parse_file(fname)

    #------------------------------------------------------------------------
    def __str__(self):
        """Return a string representation of the Config instance.
           This is just the concatenation of all the items stored in the
           attribute 'contents'.
        """
        return '\n'.join(map(str, self.contents))

    #------------------------------------------------------------------------
    def parse_file(self, fname):
        """Parse all the lines of file 'fname' by applying the method
           'parser_lines' on the file contents.
        """
        with open(fname) as f:
            self.parse_lines(f)
        return self

    #------------------------------------------------------------------------
    def parse_lines(self, lines):
        """Parse all the lines of iterable 'lines' by invoking the method
           'parse_line' for each line in 'lines'.
        """
        for line in lines:
            self.parse_line(line)
        return self

    #------------------------------------------------------------------------
    def parse_line(self, line):
        """Parse the line 'line', looking for options.
           If an option line is found, spaces are stripped from the start and
           the end of 'line' and any non-printable character is removed as well.
           Only the first occurrence of an option is processed, all the other
           occurrences are ignored. A valid option is added to the instance
           attribute 'contents' (in order to preserve its position among the
           other lines). It is also added to the mapping stored in the instance
           attribute 'options'.
           Any non-option string is added the the instance attribute 'contents',
           except those lines starting with the string stored into the instance
           attribute 'disabled_prefix' which are not followed by any option
           name.
        """
        s = ''.join(c for c in line.strip() if c in string.printable)
        moOPTION = self.creOPTION.match(s)
        if moOPTION:
            name = moOPTION.group('name').upper()
            if not name in self.options:
                self.add_option(name, moOPTION.group('value'),
                                moOPTION.group('disabled'))
        else:
            if not s.startswith(self.disabled_prefix):
                self.contents.append(line.rstrip())
        return self

    #------------------------------------------------------------------------
    def add_option(self, name, value=None, disabled=False):
        """Create a new Option instance, named 'name' (always converted to
           uppercase) with value 'value' and set its disabled status to
           'disabled'.
           The Option instance is added to the instance attribute 'contents'.
           It is also added to the mapping stored in the instance attribute
           'options'.
        """
        name = name.upper()
        opt = Option(name, value, disabled)
        self.options[name] = opt
        self.contents.append(opt)
        return opt

    #------------------------------------------------------------------------
    def set_option(self, name, value=None, disabled=False):
        """Look for an option named 'name' (always converted to
           uppercase) among the options stored in the instance
           attribute 'options'.
           If it is not found, a new Option instance is created.
           In any case its value is set to 'value' and its disabled
           status to 'disabled'.
        """
        name = name.upper()
        opt = self.options.get(name)
        if opt:
            opt.value = value
            opt.disabled = disabled
        else:
            opt = self.add_option(name, value, disabled)
        return opt

    #------------------------------------------------------------------------
    def enable_option(self, name, value=None):
        """Enable the option named 'name' (always converted to
           uppercase) and set its value to 'value'.
           If the option is not found, it is created and added to the
           end of the instance attribute 'contents'.
        """
        return self.set_option(name, value, False)

    #------------------------------------------------------------------------
    def disable_option(self, name, value=None):
        """Disable the option named 'name' (always converted to
           uppercase) and set its value to 'value'.
           If the option is not found, it is created and added to the
           end of the instance attribute 'contents'.
        """
        return self.set_option(name, value, True)

    #------------------------------------------------------------------------
    def get_option(self, name):
        """Return the value of the option named 'name' (always
           converted to uppercase).
           If the option is not found in the instance attribute
           'options', None is returned. If the stored value is None,
           it is regarded to as a boolean option and its enable status
           is returned. Otherwise its value is returned.
        """
        opt = self.options.get(name.upper())
        value = opt.get() if opt else None
        return value


#----------------------------------------------------------------------------
# MAIN
#----------------------------------------------------------------------------
if __name__ == '__main__':
    import sys
    cfg = Config(sys.argv[1] if len(sys.argv) > 1 else None)
    cfg.disable_option('needspeeling')
    cfg.enable_option('seedsremoved')
    cfg.enable_option('numberofbananas', 1024)
    cfg.enable_option('numberofstrawberries', 62000)
    print cfg

```


Output:

```txt

# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024

NUMBEROFSTRAWBERRIES 62000

```



## Racket


Use the shared <tt>[[Racket/Options|options.rkt]]</tt> code.


```Racket

#lang racket

(require "options.rkt")


(read-options "options-file")
(define-options needspeeling seedsremoved numberofbananas numberofstrawberries)

;; Disable the needspeeling option (using a semicolon prefix)
(set! needspeeling #f)
;; Enable the seedsremoved option by removing the semicolon and any
;; leading whitespace
(set! seedsremoved ENABLE)
;; Change the numberofbananas parameter to 1024
(set! numberofbananas 1024)
;; Enable (or create if it does not exist in the file) a parameter for
;; numberofstrawberries with a value of 62000
(set! numberofstrawberries 62000)

```



## REXX


### version 1

This REXX program was written to be executed under the Microsoft version of DOS   (with or without Windows).

Method note:
:* records are written (if appropriate) to a temporary file   (in the     \TEMP     folder)
:* if any changes (or deletions) were made, the original file is replaced with the temporary file
:* if no changes were made, the original file is left intact
:* the temporary file is always deleted on completion of the REXX program's execution

Programming note:   not all REXXes support the closing of files using the   '''lineout'''   BIF with a single argument.

```rexx
/*REXX program demonstrates how to  update  a configuration file  (four specific tasks).*/
parse arg iFID oFID .                            /*obtain optional arguments from the CL*/
if iFID=='' | iFID==","  then iFID=      'UPDATECF.TXT'  /*Not given?  Then use default.*/
if oFID=='' | oFID==","  then oFID='\TEMP\UPDATECF.$$$'  /* "    "       "   "     "    */
call lineout iFID;  call lineout oFID            /*close the input and the output files.*/
$.=0                                             /*placeholder of the options detected. */
call dos  'ERASE'  oFID                          /*erase a file (with no error message).*/
changed=0                                        /*nothing changed in the file (so far).*/
                                                 /* [↓]  read the entire  config  file. */
  do rec=0  while lines(iFID)\==0                /*read a record; bump the record count.*/
  z=linein(iFID);          zz=space(z)           /*get record;  elide extraneous blanks.*/
  say '───────── record:'  z                     /*echo the record just read ──► console*/
  a=left(zz,1);  _=space( translate(zz, ,';') )  /*_:  is used to elide multiple  ";"   */
  if zz=='' | a=='#'  then do; call cpy z; iterate; end            /*blank or a comment.*/
  if _==''  then do; changed=1; iterate; end     /*elide any semicolons;  empty records.*/
  parse upper var z op .                         /*obtain the option from the record.   */
                                                 /* [↓]   option may have leading or ···*/
  if a==';'  then do;   parse upper var z 2 op .                      /*trailing blanks.*/
                  if op='SEEDSREMOVED'  then call new space( substr(z, 2) )
                  call cpy z;  $.op=1            /*write the Z record to the output file*/
                  iterate  /*rec*/               /* ··· and then go read the next record*/
                  end
  if $.op  then do;  changed=1;  iterate;  end   /*is the  option  already defined?     */
  $.op=1                                         /* [↑]  Yes?   Then delete it.         */
  if op=='NEEDSPEELING'          then call new  ";"    z
  if op=='NUMBEROFBANANAS'       then call new  op  1024
  if op=='NUMBEROFSTRAWBERRIES'  then call new  op  62000
  call cpy z                                     /*write the Z record to the output file*/
  end   /*rec*/

     nos='NUMBEROFSTRAWBERRIES'                  /* [↓]  Does NOS option need updating? */
if \$.nos   then do;  call new nos 62000;  call cpy z;  end             /*update option.*/
call lineout iFID;    call lineout oFID          /*close the input and the output files.*/
if rec==0   then do;  say "ERROR:  input file wasn't found:"  iFID;  exit;  end
if changed  then do                              /*possibly overwrite the input file.   */
                 call dos 'XCOPY' oFID iFID '/y /q',">nul"                     /*quietly*/
                 say;   say center('output file', 79, "▒")                     /*title. */
                 call dos 'TYPE'  oFID           /*display content of the output file.  */
                 end
call dos 'ERASE'  oFID                           /*erase a file (with no error message).*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cpy:  call lineout oFID, arg(1);        return   /*write one line of text ───► oFID.    */
dos:  ''arg(1) word(arg(2) "2>nul",1);  return   /*execute a  DOS  command  (quietly).  */
new:  z=arg(1);    changed=1;           return   /*use new Z, indicate changed record.  */
```

'''output'''   when using the default input file (which has additional removable statements) and input options:

```txt

───────── record: # This is a configuration file in standard configuration file format
───────── record: #
───────── record: # Lines beginning with a hash or a semicolon are ignored by the application
───────── record: # program. Blank lines are also ignored by the application program.
───────── record: ;
───────── record: ;;;
───────── record: ; ; ;
───────── record: ; ;; ;
───────── record:
───────── record: # The first word on each non comment line is the configuration option.
───────── record: # Remaining words or numbers on the line are configuration parameter
───────── record: # data fields.
───────── record:
───────── record: # Note that configuration option names are not case sensitive. However,
───────── record: # configuration parameter data is case sensitive and the lettercase must
───────── record: # be preserved.
───────── record:
───────── record: # This is a favourite fruit
───────── record: FAVOURITEFRUIT banana
───────── record: FAVOURITEFRUIT not bananas
───────── record:
───────── record: # This is a boolean that should be set
───────── record: NEEDSPEELING
───────── record:
───────── record: # This boolean is commented out
───────── record: ; SEEDSREMOVED
───────── record:
───────── record: # How many bananas we have
───────── record: NUMBEROFBANANAS 48
───────── record: NUMBEROFBANANAS forty-eight

▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒output file▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000

```



### version 2


```rexx
fid='updatecf.txt'
oid='updatecf.xxx'; 'erase' oid
options=translate('FAVOURITEFRUIT NEEDSPEELING SEEDSREMOVED NUMBEROFBANANAS numberofstrawberries')
done.=0
Do While lines(fid)>0
  l=linein(fid)
  c=left(l,1)
  option=''
  If c='#' | l='' Then
    call o l
  Else Do
    If c=';' Then l=substr(l,3)
    Parse Upper Var l option value
    Select
      When option='NEEDSPEELING' Then
        Call o ';' option
      When option='SEEDSREMOVED' Then
        Call o option
      When option='NUMBEROFBANANAS' Then
        Call o option 1024
      When option='FAVOURITEFRUIT' Then
        Call o l
      When option='NUMBEROFSTRAWBERRIES' Then
        Call o option 62000
      Otherwise
        Call o '>>>' l
      End
    End
  End
Do while options<>''
  Parse Var options option options
  If done.option=0 Then
    Call o option 62000
  End
Exit
o:
If option<>'' & done.option Then
  Say 'Duplicate' option 'ignored'
Else Do
  Call lineout oid,arg(1)
  done.option=1
  End
Return
```

same as for solution 'D'


## Ruby


```ruby
require 'stringio'

class ConfigFile

  # create a ConfigFile object from a file
  def self.file(filename)
    fh = File.open(filename)
    obj = self.new(fh)
    obj.filename = filename
    fh.close
    obj
  end

  # create a ConfigFile object from a string
  def self.data(string)
    fh = StringIO.new(string)
    obj = self.new(fh)
    fh.close
    obj
  end

  def initialize(filehandle)
    @lines = filehandle.readlines
    @filename = nil
    tidy_file
  end
  attr :filename

  def save()
    if @filename
      File.open(@filename, "w") {|f| f.write(self)}
    end
  end

  def tidy_file()
    @lines.map! do |line|
      # remove leading whitespace
      line.lstrip!

      if line.match(/^#/)
        # Lines beginning with hash symbols should not be manipulated and left
        # unchanged in the revised file.
        line
      else
        # replace double semicolon prefixes with just a single semicolon
        line.sub!(/^;+\s+/, "; ")

        if line.match(/^; \s*$/)
          # Any lines beginning with a semicolon or groups of semicolons, but no
          # following option should be removed
          line = ""
        else
          # remove ... any trailing whitespace on the lines
          line = line.rstrip + "\n"

          # Whitespace between the option and paramters should consist only of a
          # single space
          if m = line.match(/^(; )?([[:upper:]]+)\s+(.*)/)
            line = (m[1].nil? ? "" : m[1]) + format_line(m[2], m[3])
          end
        end

        line
      end
    end
  end

  def format_line(option, value)
    "%s%s\n" % [option.upcase.strip, value.nil? ? "" : " " + value.to_s.strip]
  end

  # returns the index of the option, or nil if not found
  def find_option(option)
    @lines.find_index {|line| line.match(/^#{option.upcase.strip}\b/)}
  end

  # uncomments a disabled option
  def enable_option(option)
    if idx = find_option("; " + option)
      @lines[idx][/^; /] = ""
    end
  end

  # comment a line with a semi-colon
  def disable_option(option)
    if idx = find_option(option)
      @lines[idx][/^/] = "; "
    end
  end

  # add an option, or change the value of an existing option.
  # use nil for the value to set a boolean option
  def set_value(option, value)
    if idx = find_option(option)
      @lines[idx] = format_line(option, value)
    else
      @lines << format_line(option, value)
    end
  end

  def to_s
    @lines.join('')
  end
end


config = ConfigFile.data(DATA.read)
config.disable_option('needspeeling')
config.enable_option('seedsremoved')
config.set_value('numberofbananas', 1024)
config.set_value('numberofstrawberries', 62000)
puts config


__END__
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT 		banana

# This is a boolean that should be set
  NEEDSPEELING

# This boolean is commented out
;;; SEEDSREMOVED
;;;

# How many bananas we have
NUMBEROFBANANAS 48
```

outputs

```txt
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024

NUMBEROFSTRAWBERRIES 62000
```



## Tcl

Creating this to be a general solution:

```tcl
package require Tcl 8.6
oo::class create Config {
    variable filename contents
    constructor fileName {
	set filename $fileName
	set contents {}
	try {
	    set f [open $filename]
	    ### Sanitize during input
	    foreach line [split [read $f] \n] {
		if {[string match "#*" $line]} {
		    lappend contents $line
		    continue
		}
		if {[regexp {^;\W*$} $line]} continue
		set line [string trim [regsub -all {[^\u0020-\u007e]} $line {}]]
		if {[regexp {^(\W*)(\w+)(.*)$} $line -> a b c]} {
		    set line "[regsub -all {^;+} $a {;}][string toupper $b]$c"
		}
		lappend contents $line
	    }
	} finally {
	    if {[info exists f]} {
		close $f
	    }
	}
    }
    method save {} {
	set f [open $filename w]
	puts $f [join $contents \n]
	close $f
    }

    # Utility methods (not exposed API)
    method Transform {pattern vars replacement} {
	set matched 0
	set line -1
	set RE "(?i)^$pattern$"
	foreach l $contents {
	    incr line
	    if {[uplevel 1 [list regexp $RE $l -> {*}$vars]]} {
		if {$matched} {
		    set contents [lreplace $contents $line $line]
		    incr line -1
		} else {
		    lset contents $line [uplevel 1 [list subst $replacement]]
		}
		set matched 1
	    }
	}
	return $matched
    }
    method Format {k v} {
	set v " [string trimleft $v]"
	return "[string toupper $k][string trimright $v]"
    }

    # Public API for modifying options
    method enable {option} {
	if {![my Transform ";?\\s*($option)\\M\s*(.*)" {k v} \
		  {[my Format $k $v]}]} {
	    lappend contents [my Format $option ""]
	}
    }
    method disable {option} {
	if {![my Transform ";?\\s*($option)\\M\s*(.*)" {k v} \
		  {; [my Format $k $v]}]} {
	    lappend contents "; [my Format $option ""]"
	}
    }
    method set {option {value ""}} {
	if {![my Transform ";?\\s*($option)\\M.*" k {[my Format $k $value]}]} {
	    lappend contents [my Format $option $value]
	}
    }
}
```

Applying to the task at hand (assuming a file in the current directory called <tt>sample.cfg</tt>):

```tcl
set cfg [Config new "sample.cfg"]
$cfg disable needspeeling
$cfg enable seedsremoved
$cfg set numberofbananas 1024
$cfg set numberofstrawberries 62000
$cfg save
```



## TXR


This is a general solution which implements a command-line tool for updating the config file.
Omitted are the trivial steps for writing the configuration back into the same file; the final result is output
on standard output.

The first argument is the name of the config file. The remaining arguments are of this form:


```txt
  VAR      # define or update VAR as a true-valued boolean
  VAR=     # ensure "; VAR" in the config file.
  VAR=VAL  # ensure "VAR VAL" in the config file
```


This works by reading the configuration into a variable, and then making multiple passes over it, using the same constructs that normally operate on files or pipes. The first 30% of the script deals with reading the configuration file and parsing each command line argument, and converting its syntax into configuration syntax, stored in <code>new_opt_line</code>. For each argument, the configuration is then scanned and filtered from <code>config</code> to <code>new_config</code>, using the same syntax which could be used to do the same job with temporary files. When the interesting variable is encountered in the config, using one of the applicable pattern matches, then the prepared configuration line is substituted for it. While this is going on, the encountered variable names (bindings for <code>var_other</code>) are also being collected into a list. This list is then later used to check via the directive <code>@(bind opt_there option)</code> to determine whether the option occurred in the configuration or not. The bind construct will not only check whether the left and right hand side are equal, but if nested lists are involved, it checks whether either side occurs in the other as a subtree. <code>option</code> binds with <code>opt_other</code> if it matches one of the option names in <code>opt_other</code>. Finally, the updated config is regurgitated.


```txr
@(next :args)
@configfile
@(maybe)
@  (next configfile)
@  (collect :vars (config))
@config
@  (end)
@(end)
@(collect)
@  (cases)
@option=
@    (output :into new_opt_line :filter :upcase)
; @option
@    (end)
@  (or)
@option=@val
@    (output :into new_opt_line :filter :upcase)
@option @val
@    (end)
@  (or)
@option
@    (output :into new_opt_line :filter :upcase)
@option
@    (end)
@  (end)
@  (next :var config)
@  (local new_config)
@  (bind new_config ())
@  (collect :vars ((opt_there "")))
@  (block)
@    (cases)
@      (cases)
@{line /[ \t]*/}
@      (or)
@{line /#.*/}
@      (end)
@      (output :append :into new_config)
@line
@      (end)
@      (accept)
@    (or)
@      (maybe)
; @opt_there
@      (or)
@opt_there @(skip)
@      (or)
@opt_there
@      (or)
@original_line
@      (end)
@    (end)
@    (cases)
@      (bind opt_there option :filter :upcase)
@      (output :append :into new_config)
@new_opt_line
@      (end)
@    (or)
@      (output :append :into new_config)
@original_line
@      (end)
@    (end)
@  (end)
@  (cases)
@    (bind opt_there option :filter :upcase)
@  (or)
@    (output :append :into new_config)
@new_opt_line
@    (end)
@  (end)
@  (set config new_config)
@(end)
@(output)
@  (repeat)
@config
@  (end)
@(end)
```


Sample invocation:


```txt
$ txr configfile2.txr configfile NEEDSPEELING= seedsREMOVED NUMBEROFBANANAS=1024 NUMBEROFSTRAWBERRIES=62000
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000
```


Test run on empty input:


```txt
$ echo -n | txr configfile2.txr - NEEDSPEELING= SEEDSREMOVED NUMBEROFBANANAS=1024 NUMBEROFSTRAWBERRIES=62000
; NEEDSPEELING
SEEDSREMOVED
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000
```


Test run on empty input with no arguments


```txt
$ echo -n | txr configfile2.txr -
[ no output ]
```



## VBScript


```vb

Set objFSO = CreateObject("Scripting.FileSystemObject")

'Paramater lookups
Set objParamLookup = CreateObject("Scripting.Dictionary")
With objParamLookup
	.Add "FAVOURITEFRUIT", "banana"
	.Add "NEEDSPEELING", ""
	.Add "SEEDSREMOVED", ""
	.Add "NUMBEROFBANANAS", "1024"
	.Add "NUMBEROFSTRAWBERRIES", "62000"
End With

'Open the config file for reading.
Set objInFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
	"\IN_config.txt",1)
'Initialize output.
Output = ""
Isnumberofstrawberries = False
With objInFile
	Do Until .AtEndOfStream
		line = .ReadLine
		If Left(line,1) = "#" Or line = "" Then
			Output = Output & line & vbCrLf
		ElseIf Left(line,1) = " " And InStr(line,"#") Then
			Output = Output & Mid(line,InStr(1,line,"#"),1000) & vbCrLf
		ElseIf Replace(Replace(line,";","")," ","") <> "" Then
			If InStr(1,line,"FAVOURITEFRUIT",1) Then
				Output = Output & "FAVOURITEFRUIT" & " " & objParamLookup.Item("FAVOURITEFRUIT") & vbCrLf
			ElseIf InStr(1,line,"NEEDSPEELING",1) Then
				Output = Output & "; " & "NEEDSPEELING" & vbCrLf
			ElseIf InStr(1,line,"SEEDSREMOVED",1) Then
				Output = Output & "SEEDSREMOVED" & vbCrLf
			ElseIf InStr(1,line,"NUMBEROFBANANAS",1) Then
				Output = Output & "NUMBEROFBANANAS" & " " & objParamLookup.Item("NUMBEROFBANANAS") & vbCrLf
			ElseIf InStr(1,line,"NUMBEROFSTRAWBERRIES",1) Then
				Output = Output & "NUMBEROFSTRAWBERRIES" & " " & objParamLookup.Item("NUMBEROFSTRAWBERRIES") & vbCrLf
				Isnumberofstrawberries = True
			End If
		End If
	Loop
	If Isnumberofstrawberries = False Then
		Output = Output & "NUMBEROFSTRAWBERRIES" & " " & objParamLookup.Item("NUMBEROFSTRAWBERRIES") & vbCrLf
		Isnumberofstrawberries = True
	End If
	.Close
End With

'Create a new config file.
Set objOutFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
	"\OUT_config.txt",2,True)
With objOutFile
	.Write Output
	.Close
End With

Set objFSO = Nothing
Set objParamLookup = Nothing

```


```txt

# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
    # be preserved.

# This is a favourite fruit
FAVOURITEFRUIT    ßßßßß		banana    µµµµ

# This is a boolean that should be set
NEEDspeeling

# This boolean is commented out
;;;; SEEDSREMOVED µµµµµ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

# How many bananas we have
    NUMBEROFBANANAS µµµµµ 48

```


```txt

# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
; NEEDSPEELING

# This boolean is commented out
SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 1024
NUMBEROFSTRAWBERRIES 62000

```

