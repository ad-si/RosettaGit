+++
title = "Read a configuration file"
description = ""
date = 2019-09-30T01:10:39Z
aliases = []
[extra]
id = 9244
[taxonomies]
categories = []
tags = []
+++

{{task|File handling}}

The task is to read a configuration file in standard configuration file format, 
and set variables accordingly. 

For this task, we have a configuration file as follows:

 # This is a configuration file in standard configuration file format
 #
 # Lines beginning with a hash or a semicolon are ignored by the application
 # program. Blank lines are also ignored by the application program.
 
 # This is the fullname parameter
 FULLNAME Foo Barber
 
 # This is a favourite fruit
 FAVOURITEFRUIT banana
 
 # This is a boolean that should be set
 NEEDSPEELING
 
 # This boolean is commented out
 ; SEEDSREMOVED
 
 # Configuration option names are not case sensitive, but configuration parameter
 # data is case sensitive and may be preserved by the application program.
 
 # An optional equals sign can be used to separate configuration parameter data
 # from the option name. This is dropped by the parser. 
 
 # A configuration option may take multiple parameters separated by commas.
 # Leading and trailing whitespace around parameter names and parameter data fields
 # are ignored by the application program.
 
 OTHERFAMILY Rhu Barber, Harry Barber


For the task we need to set four variables according to the configuration entries as follows:

*fullname = Foo Barber
*favouritefruit = banana
*needspeeling = true
*seedsremoved = false


We also have an option that contains multiple parameters. These may be stored in an array.

* otherfamily(1) = Rhu Barber
* otherfamily(2) = Harry Barber


;Related tasks
* [[Update a configuration file]]





## Ada


{{works with|Ada|2005}}
Uses package Config available at SourceForge: 
https://sourceforge.net/projects/ini-files/

```Ada
with Config; use Config;
with Ada.Text_IO; use Ada.Text_IO;

procedure Rosetta_Read_Cfg is
  cfg: Configuration:= Init("rosetta_read.cfg", Case_Sensitive => False, Variable_Terminator => ' ');
  fullname       : String  := cfg.Value_Of("*", "fullname");
  favouritefruit : String  := cfg.Value_Of("*", "favouritefruit");
  needspeeling   : Boolean := cfg.Is_Set("*", "needspeeling");
  seedsremoved   : Boolean := cfg.Is_Set("*", "seedsremoved");
  otherfamily    : String  := cfg.Value_Of("*", "otherfamily");
begin
  Put_Line("fullname = "       & fullname);
  Put_Line("favouritefruit = " & favouritefruit);
  Put_Line("needspeeling = "   & Boolean'Image(needspeeling));
  Put_Line("seedsremoved = "   & Boolean'Image(seedsremoved));
  Put_Line("otherfamily = "    & otherfamily);
end;
```


{{out}}

```txt

fullname = Foo Barber
favouritefruit = banana
needspeeling = TRUE
seedsremoved = FALSE
otherfamily = Rhu Barber, Harry Barber
```



## Aime


```aime
record r, s;
integer c;
file f;
list l;
text an, d, k;

an = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

f.affix("tmp/config");

while ((c = f.peek) ^ -1) {
    integer removed;

    f.side(" \t\r");
    c = f.peek;
    removed = c == ';';
    if (removed) {
        f.pick;
        f.side(" \t\r");
        c = f.peek;
    }
    c = place(an, c);
    if (-1 < c && c < 52) {
        f.near(an, k);
        if (removed) {
            r[k] = "false";
        } else {
            f.side(" \t\r");
            if (f.peek == '=') {
                f.pick;
                f.side(" \t\r");
            }
            f.ever(",#\n", d);
            d = bb_drop(d, " \r\t");
            if (f.peek != ',') {
                r[k] = ~d ? d : "true";
            } else {
                f.news(l, 0, 0, ",");
                lf_push(l, d);
                for (c, d in l) {
                    l[c] = bb_drop(d, " \r\t").bf_drop(" \r\t").string;
                }
                s.put(k, l);
                f.seek(-1, SEEK_CURRENT);
            }
        }
    }

    f.slip;
}

r.wcall(o_, 0, 2, ": ", "\n");

for (k, l in s) {
    o_(k, ": ");
    l.ucall(o_, 0, ", ");
    o_("\n");
}
```

{{Out}}

```txt
FAVOURITEFRUIT: banana
FULLNAME: Foo Barber
NEEDSPEELING: true
SEEDSREMOVED: false
OTHERFAMILY: Rhu Barber, Harry Barber,
```



## AutoHotkey



```AutoHotkey

; Author: AlephX, Aug 18 2011 
data = %A_scriptdir%\rosettaconfig.txt
comma := ","

Loop, Read, %data%
	{
	if NOT (instr(A_LoopReadLine, "#") == 1 OR A_LoopReadLine == "")

		{
		if instr(A_LoopReadLine, ";") == 1
			{
			parameter := RegExReplace(Substr(A_LoopReadLine,2), "^[ \s]+|[ \s]+$", "")
			%parameter% = "1"		
			}
		else
			{
			parameter := RegExReplace(A_LoopReadLine, "^[ \s]+|[ \s]+$", "")
			
			if instr(parameter, A_Space)
				{
				value := substr(parameter, instr(parameter, A_Space)+1,999)
				parameter := substr(parameter, 1, instr(parameter, A_Space)-1)
				
				if (instr(value, ",") <> 0)
					{
					Loop, Parse, value, %comma% ,%A_Space%
						%parameter%%A_Index% := A_Loopfield					
					}
				else
					%parameter% = %value%
				}
			else
				%parameter% = "0"
			}		
		}
	}
msgbox, FULLNAME %fullname%`nFAVOURITEFRUIT %FAVOURITEFRUIT%`nNEEDSPEELING %NEEDSPEELING%`nSEEDSREMOVED %SEEDSREMOVED%`nOTHERFAMILY %OTHERFAMILY1% + %OTHERFAMILY2%

```



## AWK


```AWK

# syntax: GAWK -f READ_A_CONFIGURATION_FILE.AWK
BEGIN {
    fullname = favouritefruit = ""
    needspeeling = seedsremoved = "false"
    fn = "READ_A_CONFIGURATION_FILE.INI"
    while (getline rec <fn > 0) {
      tmp = tolower(rec)
      if (tmp ~ /^ *fullname/) { fullname = extract(rec) }
      else if (tmp ~ /^ *favouritefruit/) { favouritefruit = extract(rec) }
      else if (tmp ~ /^ *needspeeling/) { needspeeling = "true" }
      else if (tmp ~ /^ *seedsremoved/) { seedsremoved = "true" }
      else if (tmp ~ /^ *otherfamily/) { split(extract(rec),otherfamily,",") }
    }
    close(fn)
    printf("fullname=%s\n",fullname)
    printf("favouritefruit=%s\n",favouritefruit)
    printf("needspeeling=%s\n",needspeeling)
    printf("seedsremoved=%s\n",seedsremoved)
    for (i=1; i<=length(otherfamily); i++) {
      sub(/^ +/,"",otherfamily[i]) # remove leading spaces
      sub(/ +$/,"",otherfamily[i]) # remove trailing spaces
      printf("otherfamily(%d)=%s\n",i,otherfamily[i])
    }
    exit(0)
}
function extract(rec,  pos,str) {
    sub(/^ +/,"",rec)       # remove leading spaces before parameter name
    pos = match(rec,/[= ]/) # determine where data begins
    str = substr(rec,pos)   # extract the data
    gsub(/^[= ]+/,"",str)   # remove leading "=" and spaces
    sub(/ +$/,"",str)       # remove trailing spaces
    return(str)
}

```

{{out}}

```txt

fullname=Foo Barber
favouritefruit=banana
needspeeling=true
seedsremoved=false
otherfamily(1)=Rhu Barber
otherfamily(2)=Harry Barber

```



## BASIC

{{works with|QBasic|1.1}}
{{works with|QuickBasic|4.5}}
{{works with|VB-DOS|1.0}}
{{works with|QB64|1.1}}

This is a fully functional program with generic reading of a configuration file with variable names up to 20 characters and values up to 30 characters. Both limits can be expanded as needed but remember how quick it can eat RAM. It can read configuration files with variables separated of values through spaces or equal sign (=), so it will find "Variable = Value" or "Variable Value". Values can be separated by commas in configuration file and it will create a virtual array. This program will omit lines begining with #, ; or null.

```qbasic

' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
' Read a Configuration File V1.0                    '
'                                                   '
' Developed by A. David Garza Marín in VB-DOS for   '
' RosettaCode. December 2, 2016.                    '
' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '

OPTION EXPLICIT  ' For VB-DOS, PDS 7.1
' OPTION _EXPLICIT  ' For QB64

' SUBs and FUNCTIONs
DECLARE FUNCTION ErrorMessage$ (WhichError AS INTEGER)
DECLARE FUNCTION YorN$ ()
DECLARE FUNCTION FileExists% (WhichFile AS STRING)
DECLARE FUNCTION ReadConfFile% (NameOfConfFile AS STRING)
DECLARE FUNCTION getVariable$ (WhichVariable AS STRING)
DECLARE FUNCTION getArrayVariable$ (WhichVariable AS STRING, WhichIndex AS INTEGER)

' Register for values located
TYPE regVarValue
  VarName AS STRING * 20
  VarType AS INTEGER ' 1=String, 2=Integer, 3=Real
  VarValue AS STRING * 30
END TYPE

' Var
DIM rVarValue() AS regVarValue, iErr AS INTEGER, i AS INTEGER, iHMV AS INTEGER
DIM otherfamily(1 TO 2) AS STRING
DIM fullname AS STRING, favouritefruit AS STRING, needspeeling AS INTEGER, seedsremoved AS INTEGER
CONST ConfFileName = "config.fil"

' ------------------- Main Program ------------------------
CLS
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
    END SELECT
    PRINT ")"
  NEXT i
  PRINT

  ' Sets required variables
  fullname = getVariable$("FullName")
  favouritefruit = getVariable$("FavouriteFruit")
  needspeeling = VAL(getVariable$("NeedSpeeling"))
  seedsremoved = VAL(getVariable$("SeedsRemoved"))
  FOR i = 1 TO 2
    otherfamily(i) = getArrayVariable$("OtherFamily", i)
  NEXT i
  PRINT "Variables requested to set values:"
  PRINT "fullname = "; fullname
  PRINT "favouritefruit = "; favouritefruit
  PRINT "needspeeling = ";
  IF needspeeling = 0 THEN PRINT "false" ELSE PRINT "true"
  PRINT "seedsremoved = ";
  IF seedsremoved = 0 THEN PRINT "false" ELSE PRINT "true"
  FOR i = 1 TO 2
    PRINT "otherfamily("; i; ") = "; otherfamily(i)
  NEXT i
ELSE
  PRINT ErrorMessage$(iErr)
END IF
' --------- End of Main Program -----------------------

END

FileError:
  iErr = ERR
RESUME NEXT

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

FUNCTION getArrayVariable$ (WhichVariable AS STRING, WhichIndex AS INTEGER)
  ' Var
  DIM i AS INTEGER, iHMV AS INTEGER, iCount AS INTEGER
  DIM sVar AS STRING, sVal AS STRING, sWV AS STRING
  SHARED rVarValue() AS regVarValue

  ' Looks for a variable name and returns its value
  iHMV = UBOUND(rVarValue)
  sWV = UCASE$(LTRIM$(RTRIM$(WhichVariable)))
  sVal = ""
  DO
    i = i + 1
    sVar = UCASE$(RTRIM$(rVarValue(i).VarName))
    IF sVar = sWV THEN
      iCount = iCount + 1
      IF iCount = WhichIndex THEN
        sVal = LTRIM$(RTRIM$(rVarValue(i).VarValue))
      END IF
    END IF
  LOOP UNTIL i >= iHMV OR sVal <> ""

  ' Found it or not, it will return the result.
  ' If the result is "" then it didn't found the requested variable.
  getArrayVariable$ = sVal

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

FUNCTION ReadConfFile% (NameOfConfFile AS STRING)
  ' Var
  DIM iFile AS INTEGER, iType AS INTEGER, iVar AS INTEGER, iHMV AS INTEGER
  DIM iVal AS INTEGER, iCurVar AS INTEGER, i AS INTEGER, iErr AS INTEGER
  DIM dValue AS DOUBLE
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
          IF LEFT$(sLine, 1) <> "#" THEN  ' Is not a comment?
            IF LEFT$(sLine,1) = ";" THEN  ' It is a commented variable
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

              ' Determine the variable type of each variable found in this step
              FOR i = iCurVar TO iHMV
                GOSUB DetermineVariableType
              NEXT i
            END IF
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
RETURN

END FUNCTION

```

Run

```txt

This program reads a configuration file and shows the result.

Default file name: config.fil

Variables found in file:
FULLNAME = Foo Barber (String)
FAVOURITEFRUIT = banana (String)
NEEDSPEELING = (Undefined)
SEEDSREMOVED = (Undefined)
OTHERFAMILY = Rhu Barber (String)
OTHERFAMILY = Harry Barber (String)

Variables requested to set values:
fullname = Foo Barber
favouritefruit = banana
needspeeling = false
seedsremoved = false
otherfamily( 1 ) = Rhu Barber
otherfamily( 2 ) = Harry Barber

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      BOOL = 1
      NAME = 2
      ARRAY = 3
      
      optfile$ = "options.cfg"
      
      fullname$ = FNoption(optfile$, "FULLNAME", NAME)
      favouritefruit$ = FNoption(optfile$, "FAVOURITEFRUIT", NAME)
      needspeeling% = FNoption(optfile$, "NEEDSPEELING", BOOL)
      seedsremoved% = FNoption(optfile$, "SEEDSREMOVED", BOOL)
      !^otherfamily$() = FNoption(optfile$, "OTHERFAMILY", ARRAY)
      
      PRINT "fullname = " fullname$
      PRINT "favouritefruit = " favouritefruit$
      PRINT "needspeeling = "; : IF needspeeling% PRINT "true" ELSE PRINT "false"
      PRINT "seedsremoved = "; : IF seedsremoved% PRINT "true" ELSE PRINT "false"
      PRINT "otherfamily(1) = " otherfamily$(1)
      PRINT "otherfamily(2) = " otherfamily$(2)
      END
      
      DEF FNoption(file$, key$, type%)
      LOCAL file%, opt$, comma%, bool%, name$, size%, !^array$()
      file% = OPENIN(file$)
      IF file% = 0 THEN = 0
      WHILE NOT EOF#file%
        opt$ = GET$#file%
        WHILE RIGHT$(opt$) = " " opt$ = LEFT$(opt$) : ENDWHILE
        IF opt$ = key$ OR LEFT$(opt$, LEN(key$)+1) = key$ + " " THEN
          opt$ = MID$(opt$, LEN(key$) + 1)
          WHILE LEFT$(opt$,1) = " " opt$ = MID$(opt$,2) : ENDWHILE
          CASE type% OF
            WHEN BOOL: bool% = TRUE : EXIT WHILE
            WHEN NAME: name$ = opt$ : EXIT WHILE
            WHEN ARRAY:
              REPEAT
                comma% = INSTR(opt$, ",", comma%+1)
                IF comma% size% += 1
              UNTIL comma% = 0
              DIM array$(size% + 1)
              size% = 0
              REPEAT
                comma% = INSTR(opt$, ",")
                IF comma% THEN
                  size% += 1
                  array$(size%) = LEFT$(opt$, comma%-1)
                  opt$ = MID$(opt$, comma%+1)
                  WHILE LEFT$(opt$,1) = " " opt$ = MID$(opt$,2) : ENDWHILE
                ENDIF
              UNTIL comma% = 0
              array$(size% + 1) = opt$
              EXIT WHILE
          ENDCASE
        ENDIF
      ENDWHILE
      CLOSE #file%
      CASE type% OF
        WHEN BOOL: = bool%
        WHEN NAME: = name$
        WHEN ARRAY: = !^array$()
      ENDCASE
      = 0
```

{{out}}

```txt

fullname = Foo Barber
favouritefruit = banana
needspeeling = true
seedsremoved = false
otherfamily(1) = Rhu Barber
otherfamily(2) = Harry Barber

```



## C


{{libheader|libconfini}}
'''optimized'''


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <confini.h>

#define rosetta_uint8_t unsigned char

#define FALSE 0
#define TRUE 1

#define CONFIGS_TO_READ 5
#define INI_ARRAY_DELIMITER ','

/* Assume that the config file represent a struct containing all the parameters to load */
struct configs {
	char *fullname;
	char *favouritefruit;
	rosetta_uint8_t needspeeling;
	rosetta_uint8_t seedsremoved;
	char **otherfamily;
	size_t otherfamily_len;
	size_t _configs_left_;
};

static char ** make_array (size_t * arrlen, const char * src, const size_t buffsize, IniFormat ini_format) {
 
	/* Allocate a new array of strings and populate it from the stringified source */
	*arrlen = ini_array_get_length(src, INI_ARRAY_DELIMITER, ini_format);
	char ** const dest = *arrlen ? (char **) malloc(*arrlen * sizeof(char *) + buffsize) : NULL;
	if (!dest) { return NULL; }
	memcpy(dest + *arrlen, src, buffsize);
	char * iter = (char *) (dest + *arrlen);
	for (size_t idx = 0; idx < *arrlen; idx++) {
		dest[idx] = ini_array_release(&iter, INI_ARRAY_DELIMITER, ini_format);
		ini_string_parse(dest[idx], ini_format);
	}
	return dest;

}

static int configs_member_handler (IniDispatch *this, void *v_confs) {

	struct configs *confs = (struct configs *) v_confs;

	if (this->type != INI_KEY) {

		return 0;

	}

	if (ini_string_match_si("FULLNAME", this->data, this->format)) {

		if (confs->fullname) { return 0; }
		this->v_len = ini_string_parse(this->value, this->format); /* Remove all quotes, if any */
		confs->fullname = strndup(this->value, this->v_len);
		confs->_configs_left_--;

	} else if (ini_string_match_si("FAVOURITEFRUIT", this->data, this->format)) {

		if (confs->favouritefruit) { return 0; }
		this->v_len = ini_string_parse(this->value, this->format); /* Remove all quotes, if any */
		confs->favouritefruit = strndup(this->value, this->v_len);
		confs->_configs_left_--;

	} else if (ini_string_match_si("NEEDSPEELING", this->data, this->format)) {

		if (~confs->needspeeling & 0x80) { return 0; }
		confs->needspeeling = ini_get_bool(this->value, TRUE);
		confs->_configs_left_--;

	} else if (ini_string_match_si("SEEDSREMOVED", this->data, this->format)) {

		if (~confs->seedsremoved & 0x80) { return 0; }
		confs->seedsremoved = ini_get_bool(this->value, TRUE);
		confs->_configs_left_--;

	} else if (!confs->otherfamily && ini_string_match_si("OTHERFAMILY", this->data, this->format)) {

		if (confs->otherfamily) { return 0; }
		this->v_len = ini_array_collapse(this->value, INI_ARRAY_DELIMITER, this->format); /* Save memory (not strictly needed) */
		confs->otherfamily = make_array(&confs->otherfamily_len, this->value, this->v_len + 1, this->format);
		confs->_configs_left_--;

	}

	/* Optimization: stop reading the INI file when we have all we need */
	return !confs->_configs_left_;

}

static int populate_configs (struct configs * confs) {

	/* Define the format of the configuration file */
	IniFormat config_format = {
		.delimiter_symbol = INI_ANY_SPACE,
		.case_sensitive = FALSE,
		.semicolon_marker = INI_IGNORE,
		.hash_marker = INI_IGNORE,
		.multiline_nodes = INI_NO_MULTILINE,
		.section_paths = INI_NO_SECTIONS,
		.no_single_quotes = FALSE,
		.no_double_quotes = FALSE,
		.no_spaces_in_names = TRUE,
		.implicit_is_not_empty = TRUE,
		.do_not_collapse_values = FALSE,
		.preserve_empty_quotes = FALSE,
		.disabled_after_space = TRUE,
		.disabled_can_be_implicit = FALSE
	};

	*confs = (struct configs) { NULL, NULL, 0x80, 0x80, NULL, 0, CONFIGS_TO_READ };

	if (load_ini_path("rosetta.conf", config_format, NULL, configs_member_handler, confs) & CONFINI_ERROR) {

		fprintf(stderr, "Sorry, something went wrong :-(\n");
		return 1;

	}

	confs->needspeeling &= 0x7F;
	confs->seedsremoved &= 0x7F;

	return 0;

}

int main () {

	struct configs confs;

	ini_global_set_implicit_value("YES", 0);

	if (populate_configs(&confs)) {

		return 1;

	}

	/* Print the configurations parsed */

	printf(

		"Full name: %s\n"
		"Favorite fruit: %s\n"
		"Need spelling: %s\n"
		"Seeds removed: %s\n",

		confs.fullname,
		confs.favouritefruit,
		confs.needspeeling ? "True" : "False",
		confs.seedsremoved ? "True" : "False"

	);

	for (size_t idx = 0; idx < confs.otherfamily_len; idx++) {

		printf("Other family[%d]: %s\n", idx, confs.otherfamily[idx]);

	}

	/* Free the allocated memory */

	#define FREE_NON_NULL(PTR) if (PTR) { free(PTR); }

	FREE_NON_NULL(confs.fullname);
	FREE_NON_NULL(confs.favouritefruit);
	FREE_NON_NULL(confs.otherfamily);

	return 0;

}
```


{{out}}

```txt
Full name: Foo Barber
Favorite fruit: banana
Need spelling: True
Seeds removed: False
Other family[0]: Rhu Barber
Other family[1]: Harry Barber
```



## C++


{{libheader|Boost}}
{{works with|Visual Studio| 2005}}
'''unoptimized'''


```cpp
#include "stdafx.h"
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <boost/tokenizer.hpp>
#include <boost/algorithm/string/case_conv.hpp>
using namespace std;
using namespace boost;

typedef boost::tokenizer<boost::char_separator<char> > Tokenizer;
static const char_separator<char> sep(" ","#;,");

//Assume that the config file represent a struct containing all the parameters to load
struct configs{
	string fullname;
	string favoritefruit;
	bool needspelling;
	bool seedsremoved;
	vector<string> otherfamily;
} conf;

void parseLine(const string &line, configs &conf)
{
	if (line[0] == '#' || line.empty())
		return;
	Tokenizer tokenizer(line, sep);
	vector<string> tokens;
	for (Tokenizer::iterator iter = tokenizer.begin(); iter != tokenizer.end(); iter++)
		tokens.push_back(*iter);
	if (tokens[0] == ";"){
		algorithm::to_lower(tokens[1]);
		if (tokens[1] == "needspeeling")
			conf.needspelling = false;
		if (tokens[1] == "seedsremoved")
			conf.seedsremoved = false;
	}
	algorithm::to_lower(tokens[0]);
	if (tokens[0] == "needspeeling")
		conf.needspelling = true;
	if (tokens[0] == "seedsremoved")
		conf.seedsremoved = true;
	if (tokens[0] == "fullname"){
		for (unsigned int i=1; i<tokens.size(); i++)
			conf.fullname += tokens[i] + " ";
		conf.fullname.erase(conf.fullname.size() -1, 1);
	}
	if (tokens[0] == "favouritefruit") 
		for (unsigned int i=1; i<tokens.size(); i++)
			conf.favoritefruit += tokens[i];
	if (tokens[0] == "otherfamily"){
		unsigned int i=1;
		string tmp;
		while (i<=tokens.size()){		
			if ( i == tokens.size() || tokens[i] ==","){
				tmp.erase(tmp.size()-1, 1);
				conf.otherfamily.push_back(tmp);
				tmp = "";
				i++;
			}
			else{
				tmp += tokens[i];
				tmp += " ";
				i++;
			}
		}
	}
}

int _tmain(int argc, TCHAR* argv[])
{
	if (argc != 2)
	{
		wstring tmp = argv[0];
		wcout << L"Usage: " << tmp << L" <configfile.ini>" << endl;
		return -1;
	}
	ifstream file (argv[1]);
	
	if (file.is_open())
		while(file.good())
		{
			char line[255];
			file.getline(line, 255);
			string linestring(line);
			parseLine(linestring, conf);
		}
	else
	{
		cout << "Unable to open the file" << endl;
		return -2;
	}

	cout << "Fullname= " << conf.fullname << endl;
	cout << "Favorite Fruit= " << conf.favoritefruit << endl;
	cout << "Need Spelling= " << (conf.needspelling?"True":"False") << endl;
	cout << "Seed Removed= " << (conf.seedsremoved?"True":"False") << endl;
	string otherFamily;
	for (unsigned int i = 0; i < conf.otherfamily.size(); i++)
		otherFamily += conf.otherfamily[i] + ", ";
	otherFamily.erase(otherFamily.size()-2, 2);
	cout << "Other Family= " << otherFamily << endl;

	return 0;
}

```


{{out}}

```txt
Fullname= Foo Barber
Favorite Fruit= banana
Need Spelling= True
Seed Removed= False
Other Family= Rhu Barber, Harry Barber
```


'''Solution without Boost libraries. No optimisation.'''

```cpp>#include <iostream

#include <iomanip>
#include <string>
#include <exception>
#include <fstream>
#include <vector>
#include <algorithm>

struct confi {
	std::string fullname;
	std::string favouritefruit;
	bool needspeeling;
	bool seedsremoved;
	std::vector<std::string> otherfamily;
};

void read_config(std::ifstream& in, confi& out) {
	in.open("Config.txt");
	std::string str;
	out.needspeeling = false;
	out.seedsremoved = false;
	while(!in.eof()) {
		while(getline(in,str)) {
			std::string::size_type begin = str.find_first_not_of(" \f\t\v");
			//Skips blank lines
			if(begin == std::string::npos)
				continue;
			//Skips #
			if(std::string("#").find(str[begin]) != std::string::npos)
				continue;
			std::string firstWord;
			try {
				firstWord = str.substr(0,str.find(" "));
			}
			catch(std::exception& e) {
				firstWord = str.erase(str.find_first_of(" "),str.find_first_not_of(" "));
			}
			std::transform(firstWord.begin(),firstWord.end(),firstWord.begin(), ::toupper);
			if(firstWord == "FULLNAME")
				out.fullname = str.substr(str.find(" ")+1,str.length());
			if(firstWord == "FAVOURITEFRUIT")
				out.favouritefruit = str.substr(str.find(" ")+1,str.length());
			if(firstWord == "NEEDSPEELING")
				out.needspeeling = true;
			if(firstWord == "SEEDSREMOVED")
				out.seedsremoved = true;
			if(firstWord == "OTHERFAMILY") {
				size_t found = str.find(",");
				if(found != std::string::npos) {
					out.otherfamily.push_back(str.substr(str.find_first_of(" ")+1,found-str.find_first_of(" ")-1));
					out.otherfamily.push_back(str.substr(found+2,str.length()));
				}
			}
					
		}
	}
	std::cout << "Full Name: " << out.fullname << std::endl;
	std::cout << "Favourite Fruit: " << out.favouritefruit << std::endl;
	std::cout << "Needs peeling?: ";
	if(out.needspeeling == true)
		std::cout << "True" << std::endl;
	else
		std::cout << "False" << std::endl;
	std::cout << "Seeds removed?: ";
	if(out.seedsremoved == true)
		std::cout << "True" << std::endl;
	else
		std::cout << "False" << std::endl;
	std::cout << "Other family members: " << out.otherfamily[0] << ", " << out.otherfamily[1] << std::endl;
}
int main() {
	std::ifstream inp;
	confi outp;
	read_config(inp,outp);
}

```


{{out}}

```txt
Full Name: Foo Barber
Favourite Fruit: banana
Needs peeling?: True
Seeds removed?: False
Other family members: Rhu Barber, Harry Barber
```



## Clojure


```clojure
(ns read-conf-file.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(def conf-keys ["fullname"
                "favouritefruit"
                "needspeeling"
                "seedsremoved"
                "otherfamily"])

(defn get-lines
  "Read file returning vec of lines."
  [file]
  (try 
    (with-open [rdr (io/reader file)]
      (into [] (line-seq rdr)))
    (catch Exception e (.getMessage e))))

(defn parse-line
  "Parse passed line returning vec: token, vec of values."
  [line]
  (if-let [[_ k v] (re-matches #"(?i)^\s*([a-z]+)(?:\s+|=)?(.+)?$" line)]
    (let [k (str/lower-case k)]
      (if v
        [k (str/split v #",\s*")]
        [k [true]]))))

(defn mk-conf
  "Build configuration map from lines."
  [lines]
  (->> (map parse-line lines)
       (filter (comp not nil?))
       (reduce (fn
                 [m [k v]]
                 (assoc m k v)) {})))

(defn output
  [conf-keys conf]
  (doseq [k conf-keys]
    (let [v (get conf k)]
      (if v
        (println (format "%s = %s" k (str/join ", " v)))
        (println (format "%s = %s" k "false"))))))

(defn -main
  [filename]
  (output conf-keys (mk-conf (get-lines filename))))
```


{{out}}

```txt

fullname = Foo Barber
favouritefruit = banana
needspeeling = true
seedsremoved = false
otherfamily = Rhu Barber, Harry Barber

```



## COBOL


```cobol

       identification division.
       program-id. ReadConfiguration.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.
           select config-file     assign to "Configuration.txt"
                                  organization line sequential.
       data division.
       file section.

       fd  config-file.
       01  config-record          pic is x(128).

       working-storage section.
       77  idx                    pic 9(3).
       77  pos                    pic 9(3).
       77  last-pos               pic 9(3).
       77  config-key             pic x(32).
       77  config-value           pic x(64).
       77  multi-value            pic x(64).
       77  full-name              pic x(64).
       77  favourite-fruit        pic x(64).
       77  other-family           pic x(64) occurs 10.
       77  need-speeling          pic x(5) value "false".
       77  seeds-removed          pic x(5) value "false".

       procedure division.
       main.
           open input config-file
           perform until exit
              read config-file
                 at end
                    exit perform
              end-read  
              move trim(config-record) to config-record
              if config-record(1:1) = "#" or ";" or spaces
                 exit perform cycle
              end-if
              unstring config-record delimited by spaces into config-key
              move trim(config-record(length(trim(config-key)) + 1:)) to config-value
              if config-value(1:1) = "="
                 move trim(config-value(2:)) to config-value
              end-if
              evaluate upper-case(config-key)
                 when "FULLNAME"
                    move config-value to full-name
                 when "FAVOURITEFRUIT"
                    move config-value to favourite-fruit
                 when "NEEDSPEELING"
                    if config-value = spaces
                       move "true" to config-value
                    end-if
                    if config-value = "true" or "false"
                       move config-value to need-speeling
                    end-if
                 when "SEEDSREMOVED"
                    if config-value = spaces
                       move "true" to config-value
                    end-if,
                    if config-value = "true" or "false"
                       move config-value to seeds-removed
                    end-if
                 when "OTHERFAMILY"
                    move 1 to idx, pos
                    perform until exit
                       unstring config-value delimited by "," into multi-value with pointer pos
                          on overflow
                             move trim(multi-value) to other-family(idx)
                             move pos to last-pos
                          not on overflow
                             if config-value(last-pos:) <> spaces
                                move trim(config-value(last-pos:)) to other-family(idx)
                             end-if,
                             exit perform
                       end-unstring
                       add 1 to idx
                    end-perform
              end-evaluate
           end-perform
           close config-file

           display "fullname = " full-name
           display "favouritefruit = " favourite-fruit
           display "needspeeling = " need-speeling
           display "seedsremoved = " seeds-removed
           perform varying idx from 1 by 1 until idx > 10
              if other-family(idx) <> low-values
                 display "otherfamily(" idx ") = " other-family(idx)
              end-if
           end-perform
           .

```


{{out}}

```txt

fullname = Foo Barber
favouritefruit = banana
needspeeling = true
seedsremoved = false
otherfamily(001) = Rhu Barber
otherfamily(002) = Harry Barber

```



## Common Lisp

Using parser-combinators available in quicklisp:

```lisp
(ql:quickload :parser-combinators)

(defpackage :read-config
  (:use :cl :parser-combinators))

(in-package :read-config)

(defun trim-space (string)
  (string-trim '(#\space #\tab) string))

(defun any-but1? (except)
  (named-seq? (<- res (many1? (except? (item) except)))
              (coerce res 'string)))

(defun values? ()
  (named-seq? (<- values (sepby? (any-but1? #\,) #\,))
              (mapcar 'trim-space values)))

(defun key-values? ()
  (named-seq? (<- key (word?))
              (opt? (many? (whitespace?)))
              (opt? #\=)
              (<- values (values?))
              (cons key (or (if (cdr values) values (car values)) t))))

(defun parse-line (line)
  (setf line (trim-space line))
  (if (or (string= line "") (member (char line 0) '(#\# #\;)))
      :comment
      (parse-string* (key-values?) line)))

(defun parse-config (stream)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for line = (read-line stream nil nil)
       while line
       do (let ((parsed (parse-line line)))
            (cond ((eq parsed :comment))
                  ((eq parsed nil) (error "config parser error: ~a" line))
                  (t (setf (gethash (car parsed) hash) (cdr parsed))))))
    hash))
```

{{out}}

```lisp
READ-CONFIG> (with-open-file (s "test.cfg") (parse-config s))
#<HASH-TABLE :TEST EQUAL :COUNT 4 {100BD25B43}>
READ-CONFIG> (maphash (lambda (k v) (print (list k v))) *)

("FULLNAME" "Foo Barber") 
("FAVOURITEFRUIT" "banana") 
("NEEDSPEELING" T) 
("OTHERFAMILY" ("Rhu Barber" "Harry Barber")) 
NIL
READ-CONFIG> (gethash "SEEDSREMOVED" **)
NIL
NIL

```



## D


```d
import std.stdio, std.string, std.conv, std.regex, std.getopt;

enum VarName(alias var) = var.stringof.toUpper;

void setOpt(alias Var)(in string line) {
    auto m = match(line, regex(`^` ~ VarName!Var ~ `(\s+(.*))?`));
    if (!m.empty) {
        static if (is(typeof(Var) == string))
            Var = m.captures.length > 2 ? m.captures[2] : "";
        static if (is(typeof(Var) == bool))
            Var = true;
        static if (is(typeof(Var) == int))
            Var = m.captures.length > 2 ? to!int(m.captures[2]) : 0;
    }
}

void main(in string[] args) {
    string fullName, favouriteFruit, otherFamily;
    bool needsPeeling, seedsRemoved; // Default false.
    auto f = "readcfg.txt".File;

    foreach (line; f.byLine) {
        auto opt = line.strip.idup;
        setOpt!fullName(opt);
        setOpt!favouriteFruit(opt);
        setOpt!needsPeeling(opt);
        setOpt!seedsRemoved(opt);
        setOpt!otherFamily(opt);
    }

    writefln("%14s = %s", VarName!fullName, fullName);
    writefln("%14s = %s", VarName!favouriteFruit, favouriteFruit);
    writefln("%14s = %s", VarName!needsPeeling, needsPeeling);
    writefln("%14s = %s", VarName!seedsRemoved, seedsRemoved);
    writefln("%14s = %s", VarName!otherFamily, otherFamily);
}
```

{{out}}

```txt
     FULLNAME = Foo Barber
AVOURITEFRUIT = banana
 NEEDSPEELING = true
 SEEDSREMOVED = false
  OTHERFAMILY = Rhu Barber, Harry Barber
```



## DCL


```DCL
$ open input config.ini
$ loop:
$  read /end_of_file = done input line
$  line = f$edit( line, "trim" )  ! removes leading and trailing spaces or tabs
$  if f$length( line ) .eq. 0 then $ goto loop
$  first_character = f$extract( 0, 1, line )
$  if first_character .eqs. "#" .or. first_character .eqs. ";" then $ goto loop
$  equal_sign_offset = f$locate( "=", line )
$  length_of_line = f$length( line )
$  if equal_sign_offset .ne. length_of_line then $ line = f$extract( 0, equal_sign_offset, line ) + " " + f$extract( equal_sign_offset + 1, length_of_line, line )
$  option_name = f$element( 0, " ", line )
$  parameter_data = line - option_name - " "
$  if parameter_data .eqs. "" then $ parameter_data = "true"
$  'option_name = parameter_data
$  show symbol 'option_name
$  goto loop
$ done:
$ close input
```

{{out}}

```txt
$ @read_a_configuration_file
  FULLNAME = "Foo Barber"
  FAVOURITEFRUIT = "banana"
  NEEDSPEELING = "true"
```



## EchoLisp

{{Incorrect|EchoLisp|Makes no attempt to parse the configuration file of the task description.}}
There is no 'config file' in EchoLisp, but a '''(preferences)''' function which is automatically loaded and evaluated at boot-time, and automatically saved after modification. This function can set global parameters, or call other functions, or load libraries.

```lisp

(edit 'preferences)
;; current contents to edit is displayed in the input box
(define (preferences)
(define-syntax-rule (++ n) (begin (set! n (1+ n)) n))
(define-syntax-rule (% a b) (modulo a b))
;; (lib 'gloops)
(lib 'timer))

;; enter new preferences
(define (preferences)
    (define FULLNAME "Foo Barber")
    (define FAVOURITEFRUIT 'banana)
    (define NEEDSPELLING #t)
; SEEDSREMOVED
    (define OTHERFAMILY '("Rhu Barber" "Harry Barber")))

```

{{out}}

```lisp

;; press F5 or COMMAND-R to reload
EchoLisp - 2.13.12
📗 local-db: db.version: 13

;; enter parameters names : 
NEEDSPELLING → #t
FAVOURITEFRUIT → banana
SEEDSREMOVED
😡 error: #|user| : unbound variable : SEEDSREMOVED

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Configuration_file do
  def read(file) do
    File.read!(file)
    |> String.split(~r/\n|\r\n|\r/, trim: true)
    |> Enum.reject(fn line -> String.starts_with?(line, ["#", ";"]) end)
    |> Enum.map(fn line ->
         case String.split(line, ~r/\s/, parts: 2) do
           [option]         -> {to_atom(option), true}
           [option, values] -> {to_atom(option), separate(values)}
         end
       end)
  end
  
  def task do
    defaults = [fullname: "Kalle", favouritefruit: "apple", needspeeling: false, seedsremoved: false]
    options = read("configuration_file") ++ defaults
    [:fullname, :favouritefruit, :needspeeling, :seedsremoved, :otherfamily]
    |> Enum.each(fn x ->
         values = options[x]
         if is_boolean(values) or length(values)==1 do
           IO.puts "#{x} = #{values}"
         else
           Enum.with_index(values) |> Enum.each(fn {value,i} ->
             IO.puts "#{x}(#{i+1}) = #{value}"
           end)
         end
       end)
  end
  
  defp to_atom(option), do: String.downcase(option) |> String.to_atom
  
  defp separate(values), do: String.split(values, ",") |> Enum.map(&String.strip/1)
end

Configuration_file.task
```


{{out}}

```txt

fullname = Foo Barber
favouritefruit = banana
needspeeling = true
seedsremoved = false
otherfamily(1) = Rhu Barber
otherfamily(2) = Harry Barber

```



## Erlang



```Erlang

-module( configuration_file ).

-export( [read/1, task/0] ).

read( File ) ->
    {ok, Binary} = file:read_file( File ),
    Lines = [X || <<First:8, _T/binary>> = X <- binary:split(Binary, <<"\n">>, [global]), First =/= $#, First =/= $;],
    [option_from_binaries(binary:split(X, <<" ">>)) || X <- Lines].

task() ->
    Defaults = [{fullname, "Kalle"}, {favouritefruit, "apple"}, {needspeeling, false}, {seedsremoved, false}],
    Options = read( "configuration_file" ) ++ Defaults,
    [io:fwrite("~p = ~p~n", [X, proplists:get_value(X, Options)]) || X <- [fullname, favouritefruit, needspeeling, seedsremoved, otherfamily]].



option_from_binaries( [Option] ) -> {erlang:list_to_atom(string:to_lower(erlang:binary_to_list(Option))), true};
option_from_binaries( [Option, Values] ) -> {erlang:list_to_atom(string:to_lower(erlang:binary_to_list(Option))), option_from_binaries_value(binary:split(Values, <<", ">>))}.

option_from_binaries_value( [Value] ) -> erlang:binary_to_list(Value);
option_from_binaries_value( Values ) -> [erlang:binary_to_list(X) || X <- Values].

```


{{out}}

```txt

50> configuration_file:task().
fullname = "Foo Barber"
favouritefruit = "banana"
needspeeling = true
seedsremoved = false
otherfamily = ["Rhu Barber","Harry Barber"]

```



## Fantom



```fantom

class Main
{
  // remove the given key and an optional '=' from start of line
  Str removeKey (Str key, Str line)
  {
    remainder := line[key.size..-1].trim
    if (remainder.startsWith("="))
    {
      remainder = remainder.replace("=", "").trim
    }
    return remainder
  }

  Void main ()
  {
    // define the variables which need configuring
    fullname := ""
    favouritefruit := "" 
    needspeeling := false 
    seedsremoved := false
    Str[] otherfamily := [,]

    // loop through the file, setting variables as needed
    File(`config.dat`).eachLine |Str line|
    {
      line = line.trim
      if (line.isEmpty || line.startsWith("#") || line.startsWith(";")) 
      { 
        // do nothing for empty and comment lines
      }
      else if (line.upper.startsWith("FULLNAME"))
      {
        fullname = removeKey("FULLNAME", line)
      }
      else if (line.upper.startsWith("FAVOURITEFRUIT"))
      {
        favouritefruit = removeKey("FAVOURITEFRUIT", line)
      }
      else if (line.upper.startsWith("NEEDSPEELING"))
      {
        needspeeling = true
      }
      else if (line.upper.startsWith("SEEDSREMOVED"))
      {
        seedsremoved = true
      }
      else if (line.upper.startsWith("OTHERFAMILY"))
      {
        otherfamily = removeKey("OTHERFAMILY", line).split(',')
      }
    }

    // report results
    echo ("Full name is $fullname")
    echo ("Favourite fruit is $favouritefruit")
    echo ("Needs peeling is $needspeeling")
    echo ("Seeds removed is $seedsremoved")
    echo ("Other family is " + otherfamily.join(", "))
  }
}

```



## Forth

Forth is one of the unique languages that provides the programmer with both an extendable interpreter and an extendable compiler. This demonstration starts from the assumption that competent Forth programmers would not write an entire interpreter just to read a config file but would extend the Forth Interpreter to do the job. This approach is more representative of how Forth is used to create small domain specific languages for specific purposes.

As a result of taking this approach some liberties have been taken here from the original task. This code creates three operators for the Forth interpreter (SET, RESET, =) and uses these operators to assign values to actual variables in the Forth system. So the "=" sign here is not optional, it is required. The "=" operator parses an entire line of text and assigns it to a string variable. Set and reset assign 0 or -1 (all bits set) to an integer variable.  Using the Forth interpreter "as is" also means that all script symbols are separated by a minimum of 1 space character however this is not a great hardship.  

A more conventional version could of course be created if absolutely mandated however it would not be created in a few lines of code as this one is. 

Something worth noting is that the FORTH interpreter will halt on a syntax error in the config.txt file. If this was not the proscribed behavior for the application then the FORTH error handler would need modification. This is possible in most systems by using the system error words (abort, catch, throw) appropriately while interpreting the config file.
<lang>\ declare the configuration variables in the FORTH app
FORTH DEFINITIONS

32 CONSTANT $SIZE

VARIABLE FULLNAME       $SIZE ALLOT
VARIABLE FAVOURITEFRUIT $SIZE ALLOT
VARIABLE NEEDSPEELING
VARIABLE SEEDSREMOVED
VARIABLE OTHERFAMILY(1) $SIZE ALLOT
VARIABLE OTHERFAMILY(2) $SIZE ALLOT

: -leading  ( addr len -- addr' len' )
            begin over c@ bl = while 1 /string repeat ;   \ remove leading blanks

: trim      ( addr len -- addr len) -leading -trailing ;  \ remove blanks both ends

\ create the config file interpreter -------
VOCABULARY CONFIG                                          \ create a namespace
CONFIG DEFINITIONS                                         \ put things in the namespace
: SET     ( addr --) true swap ! ;
: RESET   ( addr --) false swap ! ;
: #        ( -- )      1 PARSE 2DROP ;                     \ parse line and throw away
: =        ( addr --)  1 PARSE trim ROT PLACE ;            \ string assignment operator
synonym ;  #                                               \ 2nd comment operator is simple

FORTH DEFINITIONS
\ this command reads and interprets the config.txt file
: CONFIGURE ( -- ) CONFIG  s" CONFIG.TXT" INCLUDED  FORTH ;
\ config file interpreter ends ------

\ tools to validate the CONFIG interpreter
: $.      ( str --)  count type ;
: BOOL.   ( ? --)    @ IF ." ON"  ELSE ." OFF" THEN ;

: .CONFIG       CR  ." Fullname       : " FULLNAME $.
                CR  ." Favourite fruit: " FAVOURITEFRUIT $.
                CR  ." Needs peeling  : " NEEDSPEELING bool.
                CR  ." Seeds removed  : " SEEDSREMOVED bool.
                CR  ." Family:"
                CR   otherfamily(1) $.
                CR   otherfamily(2) $.  ;
```

The config file would look like this

```txt

# READ this file from within FORTH with the command CONFIGURE
FULLNAME = Foo Barber
FAVOURITEFRUIT = banana

# This is a boolean that should be set
NEEDSPEELING SET

# This boolean is commented out
; SEEDSREMOVED SET

OTHERFAMILY(1) = Rhu Barber
OTHERFAMILY(2) = Harry Barber
```

Usage and result
```txt

CONFIGURE 
Compiling config.txt ok
.config
Fullname       : Foo Barber
Favourite fruit: banana
Needs peeling  : ON
Seeds removed  : OFF
Family:
Rhu Barber
Harry Barber ok</PRE>


## Fortran



```Fortran

program readconfig
  implicit none
  integer, parameter    :: strlen = 100
  logical               :: needspeeling = .false., seedsremoved =.false.
  character(len=strlen) :: favouritefruit = "", fullname = "", fst, snd
  character(len=strlen), allocatable :: otherfamily(:), tmp(:)
  character(len=1000)   :: line
  integer               :: lun, stat,  j, j0, j1, ii = 1, z
  integer, parameter    :: state_begin=1, state_in_fst=2, state_in_sep=3

  open(newunit=lun, file="config.ini", status="old")
  
  do 
    read(lun, "(a)", iostat=stat) line
    if (stat<0) exit
    if ((line(1:1) == "#") .or. &
        (line(1:1) == ";") .or. &
        (len_trim(line)==0)) then
      cycle
    end if
    z = state_begin
    do j = 1, len_trim(line)
      if (z == state_begin) then
        if (line(j:j)/=" ") then
          j0 = j
          z = state_in_fst
        end if
      elseif (z == state_in_fst) then
        if (index("= ",line(j:j))>0) then
          fst = lower(line(j0:j-1))
          z = state_in_sep
        end if
      elseif (z == state_in_sep) then
        if (index(" =",line(j:j)) == 0) then
          snd = line(j:)
          exit
        end if
      else
         stop "not possible to be here"
      end if
    end do
    if (z == state_in_fst) then
      fst = lower(line(j0:))
    elseif (z == state_begin) then
      cycle
    end if

    if (fst=="fullname") then
      read(snd,"(a)") fullname
    elseif (fst=="favouritefruit") then
      read(snd,"(a)") favouritefruit
    elseif (fst=="seedsremoved") then
      seedsremoved = .true.
    elseif (fst=="needspeeling") then
      needspeeling = .true.
    elseif (fst=="otherfamily") then
      j = 1; ii = 1
      do while (len_trim(snd(j:)) >0)
        j1  = index(snd(j:),",")
        if (j1==0) then
          j1 = len_trim(snd)
        else
          j1 = j + j1 - 2
        end if
        do 
          if (j>len_trim(snd)) exit
          if (snd(j:j) /= " ") exit
          j = j +1
        end do
        allocate(tmp(ii)) 
        tmp(1:ii-1) = otherfamily
        call move_alloc(tmp, otherfamily)
        read(snd(j:j1),"(a)"), otherfamily(ii)
        j = j1 + 2 
        ii = ii + 1
      end do
    else 
      print *, "unknown option '"//trim(fst)//"'"; stop
    end if
  end do
  close(lun)

  print "(a,a)","fullname = ",       trim(fullname)
  print "(a,a)","favouritefruit = ", trim(favouritefruit)
  print "(a,l)","needspeeling = ",   needspeeling
  print "(a,l)","seedsremoved = ",   seedsremoved
  print "(a,*(a,:,', '))", "otherfamily = ", &
         (trim(otherfamily(j)), j=1,size(otherfamily))

contains

pure function lower (str) result (string)
    implicit none
    character(*), intent(In) :: str
    character(len(str))      :: string
    Integer :: ic, i

    character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

    string = str
    do i = 1, len_trim(str)
        ic = index(cap, str(i:i))
        if (ic > 0) string(i:i) = low(ic:ic)
    end do
end function 

end program

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub split (s As Const String, sepList As Const String, result() As String)
  If s = "" OrElse sepList = "" Then 
     Redim result(0)
     result(0) = s
     Return
  End If
  Dim As Integer i, j, count = 0, empty = 0, length
  Dim As Integer position(Len(s) + 1)
  position(0) = 0
 
  For i = 0 To len(s) - 1
    For j = 0 to Len(sepList) - 1
      If s[i] = sepList[j] Then 
        count += 1
        position(count) = i + 1       
      End If
    Next j
  Next i
 
  Redim result(count)
  If count  = 0 Then
    result(0) = s
    Return
  End If
 
  position(count + 1) = len(s) + 1
 
  For i = 1 To count + 1  
    length = position(i) - position(i - 1) - 1 
    result(i - 1) = Mid(s, position(i - 1) + 1, length)
  Next
End Sub

Type ConfigData
  fullName As String
  favouriteFruit As String
  needsPeeling As Boolean
  seedsRemoved As Boolean
  otherFamily(Any) As String
End Type

Sub readConfigData(fileName As String, cData As ConfigData)
  Dim fileNum As Integer = FreeFile
  Open fileName For Input As #fileNum
  If err > 0 Then
    Print "File could not be opened"
    Sleep
    End
  End If
  Dim ln As String
  While Not Eof(fileNum)
    Line Input #fileNum, ln
    If ln = "" OrElse Left(ln, 1) = "#" OrElse Left(ln, 1) = ";" Then Continue While
    If UCase(Left(ln, 8)) = "FULLNAME" Then 
      cData.fullName = Trim(Mid(ln, 9), Any " =")
    ElseIf UCase(Left(ln, 14)) = "FAVOURITEFRUIT" Then
      cData.favouriteFruit = Trim(Mid(ln, 15), Any " =")
    ElseIf UCase(Left(ln, 12)) = "NEEDSPEELING" Then
      Dim s As String = Trim(Mid(ln, 13), Any " =")
      If s = ""  OrElse UCase(s) = "TRUE" Then 
        cData.needsPeeling = True
      Else
        cData.needsPeeling = False
      End If
    ElseIf UCase(Left(ln, 12)) = "SEEDSREMOVED" Then
      Dim s As String = Trim(Mid(ln, 13), Any " =")
      If s = ""  OrElse UCase(s) = "TRUE" Then 
        cData.seedsRemoved = True
      Else
        cData.seedsRemoved = False
      End If
    ElseIf UCase(Left(ln, 11)) = "OTHERFAMILY" Then
       split Mid(ln, 12), ",", cData.otherFamily()
       For i As Integer = LBound(cData.otherFamily) To UBound(cData.otherFamily)
         cData.otherFamily(i) = Trim(cData.otherFamily(i), Any " =")
       Next
    End If
  Wend
  Close #fileNum
End Sub   

Dim fileName As String = "config.txt"
Dim cData As ConfigData
readConfigData fileName, cData
Print "Full name        = "; cData.fullName
Print "Favourite fruit  = "; cData.favouriteFruit
Print "Needs peeling    = "; cData.needsPeeling
Print "Seeds removed    = "; cData.seedsRemoved
For i As Integer = LBound(cData.otherFamily) To UBound(cData.otherFamily)
  Print "Other family("; Str(i); ")  = "; cData.otherFamily(i)
Next
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Full name        = Foo Barber
Favourite fruit  = banana
Needs peeling    = true
Seeds removed    = false
Other family(0)  = Rhu Barber
Other family(1)  = Harry Barber

```



## Gambas


```gambas
Public Sub Form_Open()
Dim fullname As String = Settings["fullname", "Foo Barber"]    'If fullname is empty then use the default "Foo Barber"
Dim favouritefruit As String = Settings["favouritefruit", "banana"]
Dim needspeeling As String = Settings["needspeling", True]
Dim seedsremoved As String = Settings["seedsremoved", False]
Dim otherfamily As String[] = Settings["otherfamily", ["Rhu Barber", "Harry Barber"]]

Print fullname

'To save
Settings["fullname"] = "John Smith"
fullname = Settings["fullname"]

Print fullname

End
```

Output:

```txt

Foo Barber
John Smith

```



## Go

This make assumptions about the way the config file is supposed to be structured similar to the ones made by the Python solution.

```go
package config

import (
	"errors"
	"io"
	"fmt"
	"bytes"
	"strings"
	"io/ioutil"
)

var (
	ENONE    = errors.New("Requested value does not exist")
	EBADTYPE = errors.New("Requested type and actual type do not match")
	EBADVAL  = errors.New("Value and type do not match")
)

type varError struct {
	err error
	n   string
	t   VarType
}

func (err *varError) Error() string {
	return fmt.Sprintf("%v: (%q, %v)", err.err, err.n, err.t)
}

type VarType int

const (
	Bool VarType = 1 + iota
	Array
	String
)

func (t VarType) String() string {
	switch t {
	case Bool:
		return "Bool"
	case Array:
		return "Array"
	case String:
		return "String"
	}

	panic("Unknown VarType")
}

type confvar struct {
	Type VarType
	Val  interface{}
}

type Config struct {
	m map[string]confvar
}

func Parse(r io.Reader) (c *Config, err error) {
	c = new(Config)
	c.m = make(map[string]confvar)

	buf, err := ioutil.ReadAll(r)
	if err != nil {
		return
	}

	lines := bytes.Split(buf, []byte{'\n'})

	for _, line := range lines {
		line = bytes.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		switch line[0] {
		case '#', ';':
			continue
		}

		parts := bytes.SplitN(line, []byte{' '}, 2)
		nam := string(bytes.ToLower(parts[0]))

		if len(parts) == 1 {
			c.m[nam] = confvar{Bool, true}
			continue
		}

		if strings.Contains(string(parts[1]), ",") {
			tmpB := bytes.Split(parts[1], []byte{','})
			for i := range tmpB {
				tmpB[i] = bytes.TrimSpace(tmpB[i])
			}
			tmpS := make([]string, 0, len(tmpB))
			for i := range tmpB {
				tmpS = append(tmpS, string(tmpB[i]))
			}

			c.m[nam] = confvar{Array, tmpS}
			continue
		}

		c.m[nam] = confvar{String, string(bytes.TrimSpace(parts[1]))}
	}

	return
}

func (c *Config) Bool(name string) (bool, error) {
	name = strings.ToLower(name)

	if _, ok := c.m[name]; !ok {
		return false, nil
	}

	if c.m[name].Type != Bool {
		return false, &varError{EBADTYPE, name, Bool}
	}

	v, ok := c.m[name].Val.(bool)
	if !ok {
		return false, &varError{EBADVAL, name, Bool}
	}
	return v, nil
}

func (c *Config) Array(name string) ([]string, error) {
	name = strings.ToLower(name)

	if _, ok := c.m[name]; !ok {
		return nil, &varError{ENONE, name, Array}
	}

	if c.m[name].Type != Array {
		return nil, &varError{EBADTYPE, name, Array}
	}

	v, ok := c.m[name].Val.([]string)
	if !ok {
		return nil, &varError{EBADVAL, name, Array}
	}
	return v, nil
}

func (c *Config) String(name string) (string, error) {
	name = strings.ToLower(name)

	if _, ok := c.m[name]; !ok {
		return "", &varError{ENONE, name, String}
	}

	if c.m[name].Type != String {
		return "", &varError{EBADTYPE, name, String}
	}

	v, ok := c.m[name].Val.(string)
	if !ok {
		return "", &varError{EBADVAL, name, String}
	}

	return v, nil
}
```


Usage example:

```go
package main

import (
	"os"
	"fmt"
	"config"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Printf("Usage: %v <configfile>\n", os.Args[0])
		os.Exit(1)
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	defer file.Close()

	conf, err := config.Parse(file)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	fullname, err := conf.String("fullname")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	favouritefruit, err := conf.String("favouritefruit")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	needspeeling, err := conf.Bool("needspeeling")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	seedsremoved, err := conf.Bool("seedsremoved")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	otherfamily, err := conf.Array("otherfamily")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	fmt.Printf("FULLNAME: %q\n", fullname)
	fmt.Printf("FAVOURITEFRUIT: %q\n", favouritefruit)
	fmt.Printf("NEEDSPEELING: %q\n", needspeeling)
	fmt.Printf("SEEDSREMOVED: %q\n", seedsremoved)
	fmt.Printf("OTHERFAMILY: %q\n", otherfamily)
}
```



## Groovy


```groovy
def config = [:]
def loadConfig = { File file ->
    String regex = /^(;{0,1})\s*(\S+)\s*(.*)$/
    file.eachLine { line ->
        (line =~ regex).each { matcher, invert, key, value ->
            if (key == '' || key.startsWith("#")) return
            parts = value ? value.split(/\s*,\s*/) : (invert ? [false] : [true])
            if (parts.size() > 1) {
                parts.eachWithIndex{ part, int i -> config["$key(${i + 1})"] = part}
            } else {
                config[key] = parts[0]
            }
        }
    }
}
```

Testing:

```groovy
loadConfig new File('config.ini')
config.each { println it }
```

{{out}}

```txt
FULLNAME=Foo Barber
FAVOURITEFRUIT=banana
NEEDSPEELING=true
SEEDSREMOVED=false
OTHERFAMILY(1)=Rhu Barber
OTHERFAMILY(2)=Harry Barber
```



## Haskell



```haskell

import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = readFile "config" >>= (print . parseConfig)

parseConfig :: String -> Config
parseConfig = foldr addConfigValue defaultConfig . clean . lines
    where clean = filter (not . flip any ["#", ";", "", " "] . (==) . take 1)
          
addConfigValue :: String -> Config -> Config
addConfigValue raw config = case key of
    "fullname"       -> config {fullName      = values}
    "favouritefruit" -> config {favoriteFruit = values}
    "needspeeling"   -> config {needsPeeling  = True}
    "seedsremoved"   -> config {seedsRemoved  = True}
    "otherfamily"    -> config {otherFamily   = splitOn "," values}
    _                -> config
    where (k, vs) = span (/= ' ') raw
          key = map toLower k
          values = tail vs

data Config = Config
    { fullName      :: String
    , favoriteFruit :: String
    , needsPeeling  :: Bool
    , seedsRemoved  :: Bool
    , otherFamily   :: [String]
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config "" "" False False []

```


Or, use Data.Configfile:


```haskell

import Data.ConfigFile
import Data.Either.Utils

getSetting cp x = forceEither $ get cp "Default" x

cp <- return . forceEither =<< readfile emptyCP "name_of_configuration_file"
let username = getSetting cp "username"
    password = getSetting cp "password"

```

This works with configuration files in standard format, i.e.,
 # this is a comment
 username  = myname

=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.  However boolean values don't exist in either language.
So a variable whose value in other languages is <i>false</i> simply has no value in Icon and Unicon.
If it contains any value then it can considered as being equivalent to <i>true</i>:


```unicon
procedure main(A)
    ws := ' \t'
    vars := table()
    every line := !&input do {
        line ? {
            tab(many(ws))
            if any('#;') | pos(0) then next
            vars[map(tab(upto(ws)\1|0))] := getValue()
            }
        }
    show(vars)
end

procedure getValue()
    ws := ' \t'
    a := []
    while not pos(0) do {
        tab(many(ws))
        put(a, trim(tab(upto(',')|0)))
        move(1)
        }
    return a
end

procedure show(t)
    every pair := !sort(t) do {
        every (s := pair[1]||" = ") ||:= !pair[2] || ", "
        write(s[1:-2])
        }
end
```


Sample run on above input:


```txt

->rcf <rcf.in
favouritefruit = banana
fullname = Foo Barber
needspeeling 
otherfamily = Rhu Barber, Harry Barber
->

```

Note that <tt>seedsremoved</tt> doesn't exist.


## J
http://rosettacode.org/mw/skins/common/images/button_nowiki.png


```j
require'regex'
set=:4 :'(x)=:y'

cfgString=:4 :0
  y set ''
  (1;&,~'(?i:',y,')\s*(.*)') y&set rxapply x
)

cfgBoolean=:4 :0
  y set 0
  (1;&,~'(?i:',y,')\s*(.*)') y&set rxapply x
  if.-.0-:y do.y set 1 end.
)

taskCfg=:3 :0
  cfg=: ('[#;].*';'') rxrplc 1!:1<y
  cfg cfgString 'fullname'
  cfg cfgString 'favouritefruit'
  cfg cfgBoolean 'needspeeling'
  cfg cfgBoolean 'seedsremoved'
  i.0 0
)
```


Example use:


```j
   taskCfg 'fruit.conf'
   (,' = ',]&.do)&>;: 'fullname favouritefruit needspeeling seedsremoved'
fullname = Foo Barber  
favouritefruit = banana
needspeeling = 1       
seedsremoved = 0       
```



## J



```j
require'regex'
set=:4 :'(x)=:y'

cfgString=:4 :0
  y set ''
  (1;&,~'(?i:',y,')\s*(.*)') y&set rxapply x
)

cfgBoolean=:4 :0
  y set 0
  (1;&,~'(?i:',y,')\s*(.*)') y&set rxapply x
  if.-.0-:y do.y set 1 end.
)

taskCfg=:3 :0
  cfg=: ('[#;].*';'') rxrplc 1!:1<y
  cfg cfgString 'fullname'
  cfg cfgString 'favouritefruit'
  cfg cfgBoolean 'needspeeling'
  cfg cfgBoolean 'seedsremoved'
  i.0 0
)
```


Example use:


```j
   taskCfg 'fruit.conf'
   (,' = ',]&.do)&>;: 'fullname favouritefruit needspeeling seedsremoved'
fullname = Foo Barber  
favouritefruit = banana
needspeeling = 1       
seedsremoved = 0       
```



## JavaScript

In JavaScript using an object makes more sense than local variables.  This function takes our config file in plain text as the parameter.


```javascript
function parseConfig(config) {
    // this expression matches a line starting with an all capital word, 
    // and anything after it
    var regex = /^([A-Z]+)(.*)$/mg;
    var configObject = {};
    
    // loop until regex.exec returns null
    var match;
    while (match = regex.exec(config)) {
        // values will typically be an array with one element
        // unless we want an array
        // match[0] is the whole match, match[1] is the first group (all caps word), 
        // and match[2] is the second (everything through the end of line)
        var key = match[1], values = match[2].split(",");
        if (values.length === 1) {
            configObject[key] = values[0];
        }
        else {
            configObject[key] = values.map(function(value){
                return value.trim();
            });
        }
    }
    
    return configObject;
}  
```


The result is an object, which can be represented with this JSON.


```javascript
{
  "FULLNAME": " Foo Barber",
  "FAVOURITEFRUIT": " banana",
  "NEEDSPEELING": "",
  "OTHERFAMILY": [
    "Rhu Barber",
    "Harry Barber"
  ]
}

```



## Java

A more natural way to do this in Java would be Properties.load(InputStream) but the example data is not in the format expected by that method (equals signs are optional).

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ConfigReader {
    private static final Pattern             LINE_PATTERN = Pattern.compile( "([^ =]+)[ =]?(.*)" );
    private static final Map<String, Object> DEFAULTS     = new HashMap<String, Object>() {{
        put( "needspeeling", false );
        put( "seedsremoved", false );
    }};

    public static void main( final String[] args ) {
        System.out.println( parseFile( args[ 0 ] ) );
    }

    public static Map<String, Object> parseFile( final String fileName ) {
        final Map<String, Object> result = new HashMap<String, Object>( DEFAULTS );
        /*v*/ BufferedReader      reader = null;

        try {
            reader = new BufferedReader( new FileReader( fileName ) );
            for ( String line; null != ( line = reader.readLine() );  ) {
                parseLine( line, result );
            }
        } catch ( final IOException x ) {
            throw new RuntimeException( "Oops: " + x, x );
        } finally {
            if ( null != reader ) try {
                reader.close();
            } catch ( final IOException x2 ) {
                System.err.println( "Could not close " + fileName + " - " + x2 );
            }
        }

        return result;
    }

    private static void parseLine( final String line, final Map<String, Object> map ) {
        if ( "".equals( line.trim() ) || line.startsWith( "#" ) || line.startsWith( ";" ) )
            return;

        final Matcher matcher = LINE_PATTERN.matcher( line );

        if ( ! matcher.matches() ) {
            System.err.println( "Bad config line: " + line );
            return;
        }

        final String key   = matcher.group( 1 ).trim().toLowerCase();
        final String value = matcher.group( 2 ).trim();

        if ( "".equals( value ) ) {
            map.put( key, true );
        } else if ( -1 == value.indexOf( ',' ) ) {
            map.put( key, value );
        } else {
            final String[] values = value.split( "," );

            for ( int i = 0; i < values.length; i++ ) {
                values[ i ] = values[ i ].trim();
            }
            map.put( key, Arrays.asList( values ) );
        }
    }
}
```


{{out}}

```txt
{otherfamily=[Rhu Barber, Harry Barber], favouritefruit=banana, seedsremoved=false, needspeeling=true, fullname=Foo Barber}
```



## jq

{{works with|jq|1.5}}

In the following, in the case of collisions, the last-most specification prevails.

```jq
def parse:

  def uc: .name | ascii_upcase;
  
  def parse_boolean:
    capture( "(?<name>^[^ ] *$)" )
    | { (uc) : true };

  def parse_var_value:
    capture( "(?<name>^[^ ]+)[ =] *(?<value>[^,]+ *$)" )
    | { (uc) : .value };

  def parse_var_array:
    capture( "(?<name>^[^ ]+)[ =] *(?<value>.*)" )
    | { (uc) : (.value | sub(" +$";"") | [splits(", *")]) };

  reduce inputs as $i ({};
    if $i|length == 0 or test("^[#;]") then .
    else . + ($i | ( parse_boolean // parse_var_value // parse_var_array // {} ))
    end);

parse
```


'''Invocation'''

    $ jq -n -R -f parse.jq config.txt

{{out}}

```txt
{
  "FULLNAME": "Foo Barber",
  "FAVOURITEFRUIT": "banana",
  "NEEDSPEELING": true,
  "OTHERFAMILY": [
    "Rhu Barber",
    "Harry Barber"
  ]
}
```



## Julia

{{works with|Julia|0.6}}


```julia
function readconf(file)
    vars = Dict()
    for line in eachline(file)
        line = strip(line)
        if !isempty(line) && !startswith(line, '#') && !startswith(line, ';')
            fspace  = searchindex(line, " ")
            if fspace == 0
                vars[Symbol(lowercase(line))] = true
            else
                vname, line = Symbol(lowercase(line[1:fspace-1])), line[fspace+1:end]
                value = ',' ∈ line ? strip.(split(line, ',')) : line
                vars[vname] = value
            end
        end
    end
    for (vname, value) in vars
        eval(:($vname = $value))
    end
    return vars
end

readconf("test.conf")

@show fullname favouritefruit needspeeling otherfamily
```


{{out}}

```txt
fullname = "Foo Barber"
favouritefruit = "banana"
needspeeling = true
otherfamily = SubString{String}["Rhu Barber", "Harry Barber"]
```



## Kotlin


{{works with|Kotlin|1.0.6}}
This example is more verbose than it has to be because of increased effort in providing immutability to the configuration class. 

```scala
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

data class Configuration(val map: Map<String, Any?>) {
    val fullName: String by map
    val favoriteFruit: String by map
    val needsPeeling: Boolean by map
    val otherFamily: List<String> by map
}

fun main(args: Array<String>) {
    val lines = Files.readAllLines(Paths.get("src/configuration.txt"), StandardCharsets.UTF_8)
    val keyValuePairs = lines.map{ it.trim() }
            .filterNot { it.isEmpty() }
            .filterNot(::commentedOut)
            .map(::toKeyValuePair)

    val configurationMap = hashMapOf<String, Any>("needsPeeling" to false)
    for (pair in keyValuePairs) {
        val (key, value) = pair
        when (key) {
            "FULLNAME"       -> configurationMap.put("fullName", value)
            "FAVOURITEFRUIT" -> configurationMap.put("favoriteFruit", value)
            "NEEDSPEELING"   -> configurationMap.put("needsPeeling", true)
            "OTHERFAMILY"    -> configurationMap.put("otherFamily", value.split(" , ").map { it.trim() })
            else             -> println("Encountered unexpected key $key=$value")
        }
    }
    println(Configuration(configurationMap))
}

private fun commentedOut(line: String) = line.startsWith("#") || line.startsWith(";")

private fun toKeyValuePair(line: String) = line.split(Regex(" "), 2).let {
    Pair(it[0], if (it.size == 1) "" else it[1])
}
```



## Lasso


```Lasso
local(config = '# This is a configuration file in standard configuration file format
#
# Lines beginning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# This is the fullname parameter

FULLNAME Foo Barber

# This is a favourite fruit
FAVOURITEFRUIT = banana

# This is a boolean that should be set
NEEDSPEELING

# This boolean is commented out
; SEEDSREMOVED

# Configuration option names are not case sensitive, but configuration parameter
# data is case sensitive and may be preserved by the application program.

# An optional equals sign can be used to separate configuration parameter data
# from the option name. This is dropped by the parser.

# A configuration option may take multiple parameters separated by commas.
# Leading and trailing whitespace around parameter names and parameter data fields
# are ignored by the application program.

OTHERFAMILY Rhu Barber, Harry Barber
')
// if config is in a file collect it like this
//local(config = file('path/and/file.name') -> readstring)

define getconfig(term::string, config::string) => {

	local(
		regexp	= regexp(-find = `(?m)^` + #term + `($|\s*=\s*|\s+)(.*)$`, -input = #config, -ignorecase),
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

local(
	fullname	= getconfig('FULLNAME', #config),
	favorite	= getconfig('FAVOURITEFRUIT', #config),
	sedsremoved	= getconfig('SEEDSREMOVED', #config),
	needspeel	= getconfig('NEEDSPEELING', #config),
	otherfamily	= getconfig('OTHERFAMILY', #config)
)

#fullname
'<br />'
#favorite
'<br />'
#sedsremoved
'<br />'
#needspeel
'<br />'
#otherfamily
'<br />'
```

{{out}}

```txt
Foo Barber
banana
false
true
array(Rhu Barber, Harry Barber)
```



## Liberty BASIC


```lb

dim confKeys$(100)
dim confValues$(100)

optionCount = ParseConfiguration("a.txt")

fullName$ = GetOption$( "FULLNAME", optionCount)
favouriteFruit$ = GetOption$( "FAVOURITEFRUIT", optionCount)
needsPeeling = HasOption("NEEDSPEELING", optionCount)
seedsRemoved = HasOption("SEEDSREMOVED", optionCount)
otherFamily$ = GetOption$( "OTHERFAMILY", optionCount)  'it's easier to keep the comma-separated list as a string

print "Full name: "; fullName$
print "likes: "; favouriteFruit$
print "needs peeling: "; needsPeeling
print "seeds removed: "; seedsRemoved

print "other family:"
otherFamily$ = GetOption$( "OTHERFAMILY", optionCount)
counter = 1
while word$(otherFamily$, counter, ",") <> ""
    print counter; ". "; trim$(word$(otherFamily$, counter, ","))
    counter = counter + 1
wend
end

'parses the configuration file, stores the uppercase keys in array confKeys$ and corresponding values in confValues$
'returns the number of key-value pairs found
function ParseConfiguration(fileName$)
    count = 0
    open fileName$ for input as #f
    while not(eof(#f))
        line input #f, s$
        if not(Left$(s$,1) = "#" or Left$( s$,1) = ";" or trim$(s$) = "") then  'ignore empty and comment lines
            s$ = trim$(s$)
            key$ = ParseKey$(s$)
            value$ = trim$(Mid$(s$,len(key$) + 1))
            if Left$( value$,1) = "=" then value$ = trim$(Mid$(value$,2))  'optional =
            count = count + 1
            confKeys$(count) = upper$(key$)
            confValues$(count) = value$
        end if
    wend
    close #f
    ParseConfiguration = count
end function

function ParseKey$(s$)
    'key is the first word in s$, delimited by whitespace or =
    s$ = word$(s$, 1)
    ParseKey$ = trim$(word$(s$, 1, "="))
end function


function GetOption$( key$, optionCount)
    index = Find.confKeys( 1, optionCount, key$)
    if index > 0 then GetOption$ =(confValues$(index))
end function


function HasOption(key$, optionCount)
    HasOption = Find.confKeys( 1, optionCount, key$) > 0
end function

function Find.confKeys( Start, Finish, value$)
    Find.confKeys = -1
    for i = Start to Finish
        if confKeys$(i) = value$ then Find.confKeys = i : exit for
    next i
end function

```


{{out}}

```txt
Full name: Foo Barber
likes: banana
needs peeling: 1
seeds removed: 0
other family:
1. Rhu Barber
2. Harry Barber

```



## Lua


```lua
conf = {}

fp = io.open( "conf.txt", "r" )

for line in fp:lines() do
    line = line:match( "%s*(.+)" )
    if line and line:sub( 1, 1 ) ~= "#" and line:sub( 1, 1 ) ~= ";" then
 	option = line:match( "%S+" ):lower()
	value  = line:match( "%S*%s*(.*)" )

	if not value then
 	    conf[option] = true
	else
	    if not value:find( "," ) then
		conf[option] = value
	    else
		value = value .. ","
		conf[option] = {}
		for entry in value:gmatch( "%s*(.-)," ) do
		    conf[option][#conf[option]+1] = entry
		end
	    end
	end

    end
end

fp:close()


print( "fullname = ", conf["fullname"] )
print( "favouritefruit = ", conf["favouritefruit"] )
if conf["needspeeling"] then print( "needspeeling = true" ) else print( "needspeeling = false" ) end
if conf["seedsremoved"] then print( "seedsremoved = true" ) else print( "seedsremoved = false" ) end
if conf["otherfamily"] then
    print "otherfamily:"
    for _, entry in pairs( conf["otherfamily"] ) do
	print( "", entry )
    end
end
```



## Mathematica


```Mathematica

ClearAll[CreateVar, ImportConfig];
CreateVar[x_, y_String: "True"] := Module[{},
  If[StringFreeQ[y, ","]
   ,
   ToExpression[x <> "=" <> y]
   ,
   ToExpression[x <> "={" <> StringJoin@Riffle[StringSplit[y, ","], ","] <> "}"]
   ]
  ]
ImportConfig[configfile_String] := Module[{data},
  (*data = ImportString[configfile, "List", "Numeric" -> False];*)
  data=Import[configfile,"List","Numeric"\[Rule]False];

  data = StringTrim /@ data;
  data = Select[data, # =!= "" &];
  data = Select[data, ! StringMatchQ[#, "#" | ";" ~~ ___] &];
  data = If[! StringFreeQ[#, " "], StringSplit[#, " ", 2], {#}] & /@ data;

  CreateVar @@@ data;
 ]
ImportConfig[file]

```


=={{header|MATLAB}} / {{header|Octave}}==

This is defined as a function, parameters are returned as part of a struct. When the first line, and the assignment to return values are removed, it is a script that stores the parameters in the local workspace. 


```MATLAB
function R = readconf(configfile)
% READCONF reads configuration file. 
%
% The value of boolean parameters can be tested with 
%    exist(parameter,'var')

if nargin<1, 
   configfile = 'q.conf';
end;

fid = fopen(configfile); 
if fid<0, error('cannot open file %s\n',a); end; 

while ~feof(fid)
    line = strtrim(fgetl(fid));
    if isempty(line) || all(isspace(line)) || strncmp(line,'#',1) || strncmp(line,';',1),
	; % no operation 
    else 
	[var,tok] = strtok(line,' \t=');
	var = upper(var); 
	if any(tok==','),
		k = 1; 
		while (1)
			[val, tok]=strtok(tok,',');
			R.(var){k} = strtrim(val);  	% return value of function 
			eval(sprintf('%s{%i}=''%s'';',var,k,strtrim(val)));  % stores variable in local workspace
		if isempty(tok), break; end;
			k=k+1;
		end;
	else 
		tok = strtrim(tok);
		R.(var) = tok;		% return value of function
		eval(sprintf('%s=''%s''; ',var,tok));  % stores variable in local workspace
	end;
    end;
end; 
fclose(fid);
whos,     % shows the parameter in the local workspace 
 
```



```txt
R=readconf('file.conf')
Variables in the current scope:

  Attr Name                Size                     Bytes  Class
  
### = ====                ====                     =====  ==
 
       FAVOURITEFRUIT      1x6                          6  char
       FULLNAME            1x10                        10  char
       NEEDSPEELING        0x0                          0  char
       OTHERFAMILY         1x2                         22  cell
   f   R                   1x1                         38  struct
   f   configfile          1x6                          6  char
       fid                 1x1                          8  double
       k                   1x1                          8  double
       line                0x0                          0  char
       tok                 0x0                          0  char
       val                 1x13                        13  char
       var                 1x11                        11  char

Total is 51 elements using 122 bytes

R =

  scalar structure containing the fields:

    FULLNAME = Foo Barber
    FAVOURITEFRUIT = banana
    NEEDSPEELING = 
    OTHERFAMILY = 
    {
      [1,1] = Rhu Barber
      [1,2] = Harry Barber
    }

```

	

## OCaml


Using the library [http://homepage.mac.com/letaris/ ocaml-inifiles]:


```ocaml
#use "topfind"
#require "inifiles"
open Inifiles

let print_field ini (label, field) =
  try
    let v = ini#getval "params" field in
    Printf.printf "%s: %s\n" label v
  with Invalid_element _ ->
    Printf.printf "%s: not defined\n" label

let () =
  let ini = new inifile "./conf.ini" in
  let lst = [
    "Full name", "FULLNAME";
    "likes", "FAVOURITEFRUIT";
    "needs peeling", "NEEDSPEELING";
    "seeds removed", "SEEDSREMOVED";
  ] in
  List.iter (print_field ini) lst;

  let v = ini#getaval "params" "OTHERFAMILY" in
  print_endline "other family:";
  List.iter (Printf.printf "- %s\n") v;
;;
```


The file "conf.ini":


```txt
# This is a configuration file
#
# Lines begininning with a hash are ignored
# Blank lines are also ignored

# Leading and trailing whitespace around parameter names and
# parameter data fields are ignored

# ocaml-inifiles needs at least one section name
[params]

# This is the fullname parameter
FULLNAME = Foo Barber

# This is a favourite fruit
FAVOURITEFRUIT = banana

# This is a boolean that should be set
NEEDSPEELING = true

# This boolean is commented out
#SEEDSREMOVED = false

# A configuration option may take multiple values
# these values will be returned as a list
OTHERFAMILY = Rhu Barber
OTHERFAMILY = Harry Barber

```


{{out}}

```txt
$ ocaml conf.ml 
Full name: Foo Barber
likes: banana
needs peeling: true
seeds removed: not defined
other family:
- Harry Barber
- Rhu Barber

```



## ooRexx

Here's another way of doing this, which stores the values in a Rexx stem (array), and stores each value of a multivalued variable as a separate item:

```ooRexx

#!/usr/bin/rexx
/*.----------------------------------------------------------------------.*/
/*|readconfig: Read keyword value pairs from a configuration file into   |*/
/*|            Rexx variables.                                           |*/
/*|                                                                      |*/
/*|Usage:                                                                |*/
/*|              .-~/rosetta.conf-.                                      |*/
/*|>>-readconfig-+----------------+------------------------------------><|*/
/*|              |-configfilename-|                                      |*/
/*|              |-- -? ----------|                                      |*/
/*|              |-- -h ----------|                                      |*/
/*|              '- --help -------'                                      |*/
/*|                                                                      |*/
/*|where                                                                 |*/
/*|  configfilename                                                      |*/
/*|        is the name of the configuration file to be processed.  if not|*/
/*|        specified, ~/rosetta.conf is used.                            |*/
/*|                                                                      |*/
/*|All values retrieved from the configuration file are stored in        |*/
/*|compound variables with the stem config.  Variables with multiple     |*/
/*|values have a numeric index appended, and the highest index number    |*/
/*|is stored in the variable with index 0; e.g. if CONFIG.OTHERFAMILY.1  |*/
/*|and CONFIG.OTHERFAMILY.2 have values assigned, CONFIG.OTHERFAMILY.0 = |*/
/*|2.                                                                    |*/
/*|-?, -h or --help all cause this documentation to be displayed.        |*/
/*|                                                                      |*/
/*|This program was tested using Open Object Rexx 4.1.1.  It should work |*/
/*|with most other dialects as well.                                     |*/
/*'----------------------------------------------------------------------'*/
  call usage arg(1)
  trace normal
  signal on any name error

/* Prepare for processing the configuration file. */
  keywords = 'FULLNAME FAVOURITEFRUIT NEEDSPEELING SEEDSREMOVED OTHERFAMILY'

/* Set default values for configuration variables here */
  config_single?. = 1
  config. = ''
  config.NEEDSPEELING = 0
  config.SEEDSREMOVED = 1

/* Validate command line inputs. */
  parse arg configfilename

  if length(configfilename) = 0 then
    configfilename = '~/rosetta.conf'

  configfile = stream(configfilename,'COMMAND','QUERY EXISTS')

  if length(configfile) = 0 then
    do
      say configfilename 'was not found.'
      exit 28
    end

  signal on notready                               /* if an I/O error occurs. */

/* Open the configuration file. */
  response = stream(configfile,'COMMAND','OPEN READ SHARED')

/* Parse the contents of the configuration file into variables. */
  do while lines(configfile) > 0
    statement = linein(configfile)

    select
      when left(statement,1) = '#',
         | left(statement,1) = ';',
         | length(strip(statement)) = 0,
      then                                      /* a comment or a blank line. */
        nop                                                       /* skip it. */

      otherwise
        do
          if pos('=',word(statement,1)) > 0,
           | left(word(statement,2),1) = '=',
          then                        /* a keyword/value pair with = between. */
            parse var statement keyword '=' value

          else                             /* a keyword/value pair with no =. */
            parse var statement keyword value

          keyword = translate(strip(keyword))           /* make it uppercase. */
          single? = pos(',',value) = 0 /* a single value, or multiple values? */
          call value 'CONFIG_single?.'keyword,single?          /* remember. */

          if single? then
            do
              if length(value) > 0 then
                call value 'CONFIG.'keyword,strip(value)
            end                      /* strip keeps internal whitespace only. */

          else                            /* store each value with its index. */
            do v = 1 by 1 while length(value) > 0
              parse var value value1 ',' value

              if length(value1) > 0 then
                do
                  call value 'CONFIG.'keyword'.'v,strip(value1)
                  call value 'CONFIG.'keyword'.0',v         /* remember this. */
                end
            end
        end
    end
  end

/* Display the values of the configuration variables. */
  say 'Values associated with configuration file' configfilename':'
  say

  do while words(keywords) > 0
    parse var keywords keyword keywords

    if value('CONFIG_single?.'keyword) then
      say right(keyword,20) '= "'value('CONFIG.'keyword)'"'

    else
      do
        lastv = value('CONFIG.'keyword'.0')

        do v = 1 to lastv
          say right(keyword,20-(length(v)+2))'['v'] = "'value('CONFIG.'keyword'.'v)'"'
        end
      end
  end

  say

notready:                                           /* I/O errors come here. */
  filestatus = stream(configfile,'STATE')

  if filestatus \= 'READY' then
    say 'An I/O error occurred; the file status is' filestatus'.'

  response = stream(configfile,'COMMAND','CLOSE')

error:
/*? = sysdumpvariables() */                    /* see everything Rexx used. */
exit

usage: procedure
  trace normal

  if arg(1) = '-h',
   | arg(1) = '-?',
   | arg(1) = '--help'
  then
    do
      line = '/*|'
      say

      do l = 3 by 1 while line~left(3) = '/*|'
        line = sourceline(l)
        parse var line . '/*|' text '|*/' .
        say text
      end

      say
      exit 0
    end
return

```

{{out}}

```txt

$ readconfig.rex
Values associated with configuration file ~/rosetta.conf:

            FULLNAME = "Foo Barber"
      FAVOURITEFRUIT = "banana"
        NEEDSPEELING = "0"
        SEEDSREMOVED = "1"
      OTHERFAMILY[1] = "Rhu Barber"
      OTHERFAMILY[2] = "Harry Barber"


```


Leslie


## Pascal

{{works with|Free_Pascal}}
{{libheader|SysUtils}}

This solution makes use of FCL-STL package shipped with FPC >= 2.6.0, moreover it can be run directly like a script (just chmod +x) using the new instantfpc feature.


```Pascal
#!/usr/bin/instantfpc

{$if not defined(fpc) or (fpc_fullversion < 20600)}
  {$error FPC 2.6.0 or greater required}
{$endif}

{$mode objfpc}{$H+}

uses
  Classes,SysUtils,gvector,ghashmap;

type
  TStrHashCaseInsensitive = class
    class function hash(s: String; n: Integer): Integer;
  end;

class function TStrHashCaseInsensitive.hash(s: String; n: Integer): Integer;
var
  x: Integer;
  c: Char;
begin
  x := 0;
  for c in UpCase(s) do Inc(x,Ord(c));
  Result := x mod n;
end;

type
  TConfigValues  = specialize TVector<String>;
  TConfigStorage = class(specialize THashMap<String,TConfigValues,TStrHashCaseInsensitive>)
    destructor Destroy; override;
  end;

destructor TConfigStorage.Destroy;
var
  It: TIterator;
begin
  if Size > 0 then begin
    It := Iterator;
    repeat
      It.Value.Free;
    until not It.Next;
    It.Free;
  end;
  inherited Destroy;
end;

var
  ConfigStrings,ConfigValues: TStrings;
  ConfigStorage: TConfigStorage;
  ConfigLine,ConfigName,ConfigValue: String;
  SeparatorPos: Integer;
begin
  ConfigStrings := TStringList.Create;
  ConfigValues  := TStringList.Create;
  ConfigValues.Delimiter := ',';
  ConfigValues.StrictDelimiter := true;
  ConfigStorage := TConfigStorage.Create;

  ConfigStrings.LoadFromFile('config.test');
  for ConfigLine in ConfigStrings do begin
    if Length(ConfigLine) > 0 then begin
      case ConfigLine[1] of
        '#',';': ; // ignore
        else begin
          // look for = first
          SeparatorPos := Pos('=',ConfigLine);
          // if not found, then look for space
          if SeparatorPos = 0 then begin
            SeparatorPos := Pos(' ',ConfigLine);
          end;
          // found space
          if SeparatorPos <> 0 then begin
            ConfigName := UpCase(Copy(ConfigLine,1,SeparatorPos - 1));
            ConfigValues.DelimitedText := Copy(ConfigLine,SeparatorPos + 1,Length(ConfigLine) - SeparatorPos);
          // no = or space found, take the whole line as a key name
          end else begin
            ConfigName := UpCase(Trim(ConfigLine));
          end;
          if not ConfigStorage.Contains(ConfigName) then begin
            ConfigStorage[ConfigName] := TConfigValues.Create;
          end;
          for ConfigValue in ConfigValues do begin
            ConfigStorage[ConfigName].PushBack(Trim(ConfigValue));
          end;
        end;
      end;
    end;
  end;

  WriteLn('FULLNAME = ' + ConfigStorage['FULLNAME'][0]);
  WriteLn('FAVOURITEFRUIT = ' + ConfigStorage['FAVOURITEFRUIT'][0]);
  WriteLn('NEEDSPEELING = ' + BoolToStr(ConfigStorage.Contains('NEEDSPEELING'),true));
  WriteLn('SEEDSREMOVED = ' + BoolToStr(ConfigStorage.Contains('SEEDSREMOVED'),true));
  WriteLn('OTHERFAMILY(1) = ' + ConfigStorage['OTHERFAMILY'][0]);
  WriteLn('OTHERFAMILY(2) = ' + ConfigStorage['OTHERFAMILY'][1]);

  ConfigStorage.Free;
  ConfigValues.Free;
  ConfigStrings.Free;
end.
```


{{out}}

```txt
FULLNAME = Foo Barber
FAVOURITEFRUIT = banana
NEEDSPEELING = True
SEEDSREMOVED = False
OTHERFAMILY(1) = Rhu Barber
OTHERFAMILY(2) = Harry Barber
```



## Peloton

Despite the discussion, this task is still a bit ambiguous. I've taken it that
    * blank lines and lines beginning with # and ; should be ignored. 
    * an all uppercase word defines a configuration symbol
    * no tail means a boolean true
    * a tail including a comma means a list 
    * any other kind of tail is a string

What we end up with after processing rosetta.config are three VARs and a LST, named FAVOURITEFRUIT, FULLNAME, NEEDSPEELING and OTHERFAMILY respectively.


```sgml><@ DEFUDRLIT
__ReadConfigurationFile|
	<@ LETSCPPNTPARSRC>Data|1</@><@ OMT> read file into locally scope variable</@>
	<@ LETCGDLSTLLOSCP>List|Data</@><@ OMT> split Data into a list of lines </@>
	<@ OMT> Remove comment lines, and blank lines </@>
	<@ ACTOVRBEFLSTLIT>List|;</@>
	<@ ACTOVRBEFLSTLIT>List|#</@>
	<@ ACTRMELST>List</@>
	<@ OMT> Iterate over the lines of the list </@>
	<@ ITEENULSTLit>List|
		<@ LETVARUPTVALLSTLIT>key|...| </@>
		<@ LETVARAFTVALLSTLIT>val|...| </@>
		<@ OMT> test for an empty key (in the case of a boolean) </@>
		<@ TSTVARLIT>key|</@>
		<@ IFE><@ LETPNTVARVARLIT>val|__True</@></@>
		<@ ELS>
			<@ TSTGT0ATBVARLIT>val|,</@>
			<@ IFE><@ ACTEXEEMMCAP><&prot; LETCNDLSTLITLIT>&key;&pipe;&val;&pipe;, </&prot;></@></@>
			<@ ELS><@ LETPNTVARVARVAR>key|val</@></@>
		</@>
	</@>
</@>

<@ ACTUDRLIT>__ReadConfigurationFile|c:\rosetta.config</@>
<@ SAYVAR>FAVOURITEFRUIT</@>
<@ SAYVAR>FULLNAME</@>
<@ SAYVAR>NEEDSPEELING</@>
<@ SAYDMPLST>OTHERFAMILY</@>

```



## Perl


This is an all-singing, all-dancing version that checks the configuration file syntax and contents and raises exceptions if it fails.  (It is intentionally over-commented for pedagogical purposes.)
 

```perl
my $fullname;
my $favouritefruit;
my $needspeeling;
my $seedsremoved;
my @otherfamily;

# configuration file definition.  See read_conf_file below for explanation.
my $conf_definition = {
    'fullname'          => [ 'string', \$fullname ],
    'favouritefruit'    => [ 'string', \$favouritefruit ],
    'needspeeling'      => [ 'boolean', \$needspeeling ],
    'seedsremoved'      => [ 'boolean', \$seedsremoved ],
    'otherfamily'       => [ 'array', \@otherfamily ],
};

my $arg = shift;               # take the configuration file name from the command line
                               # (or first subroutine argument if this were in a sub)
my $file;                      # this is going to be a file handle reference
open $file, $arg or die "Can't open configuration file '$arg': $!";

read_conf_file($file, $conf_definition); 

print "fullname = $fullname\n";
print "favouritefruit = $favouritefruit\n";
print "needspeeling = ", ($needspeeling ? 'true' : 'false'), "\n";
print "seedsremoved = ", ($seedsremoved ? 'true' : 'false'), "\n";
for (my $i = 0; $i < @otherfamily; ++$i) {
    print "otherfamily(", $i + 1, ") = ", $otherfamily[$i], "\n";
}

# read_conf_file:  Given a file handle opened for reading and a configuration definition,
# read the file.
# If the configuration file doesn't match the definition, raise an exception with "die".
# The configuration definition is (a reference to) an associative array
# where the keys are the configuration variable names in all lower case
# and the values are references to arrays.
# The first element of each of these arrays is the expected type:  'boolean', 'string', or 'array';
# the second element is a reference to the variable that should be assigned the data.
sub read_conf_file {
    my ($fh, $def) = @_;        # copy parameters

    local $_;                   # avoid interfering with use of $_ in main program
    while (<$fh>) {             # read a line from $fh into $_ until end of file
        next if /^#/;           # skip "#" comments
        next if /^;/;           # skip ";" comments
        next if /^$/;           # skip blank lines
        chomp;                  # strip final newline

        $_ =~ /^\s*(\w+)\s*(.*)$/i or die "Syntax error";
        my $key = $1;
        my $rest = $2;
        $key =~ tr/[A-Z]/[a-z]/; # convert keyword to lower case

        if (!exists $def->{$key}) {
            die "Unknown keyword: '$key'";
        }

        if ($def->{$key}[0] eq 'boolean') {
            if ($rest) {
                die "Syntax error:  extra data following boolean '$key'";
            }
            ${$def->{$key}[1]} = 1;
            next;                # done with this line, go back to "while"
        }

        $rest =~ s/\s*$//;       # drop trailing whitespace
        $rest =~ s/^=\s*//;      # drop equals sign if present

        if ($def->{$key}[0] eq 'string') {
            ${$def->{$key}[1]} = $rest;
        } elsif ($def->{$key}[0] eq 'array') {
            @{$def->{$key}[1]} = split /\s*,\s*/, $rest;
        } else {
            die "Internal error (unknown type in configuration definition)";
        }
    }
}

```



## Perl 6

{{Works with|rakudo|2018.03}}


This demonstrates several interesting features of Perl 6, including full grammar support, derived grammars, alternation split across derivations, and longest-token matching that works across derivations.  It also shows off Perl 6's greatly cleaned up regex syntax.

```perl6
my $fullname;
my $favouritefruit;
my $needspeeling = False;
my $seedsremoved = False;
my @otherfamily;

grammar ConfFile {
    token TOP {
	:my $*linenum = 0;
	^ <fullline>* [$ || (\N*) { die "Parse failed at $0" } ]
    }

    token fullline {
	<?before .>
	{ ++$*linenum }
	<line>
	[ \n || { die "Parse failed at line $*linenum" } ]
    }

    proto token line() {*}

    token line:misc  { {} (\S+) { die "Unrecognized word: $0" } }

    token line:sym<comment> { ^^ [ ';' | '#' ] \N* }
    token line:sym<blank>   { ^^ \h* $$ }

    token line:sym<fullname>       {:i fullname»       <rest> { $fullname = $<rest>.trim } }
    token line:sym<favouritefruit> {:i favouritefruit» <rest> { $favouritefruit = $<rest>.trim } }
    token line:sym<needspeeling>   {:i needspeeling»    <yes> { $needspeeling = defined $<yes> } }
    token rest { \h* '='? (\N*) }
    token yes { :i \h* '='? \h*
    	[
	    || ([yes|true|1])
	    || [no|false|0] 
	    || (<?>)
	] \h*
    }
}

grammar MyConfFile is ConfFile {
    token line:sym<otherfamily>    {:i otherfamily»    <rest> { @otherfamily = $<rest>.split(',')».trim } }
}

MyConfFile.parsefile('file.cfg');

say "fullname: $fullname";
say "favouritefruit: $favouritefruit";
say "needspeeling: $needspeeling";
say "seedsremoved: $seedsremoved";
print "otherfamily: "; say @otherfamily.perl;
```

{{out}}

```txt
fullname: Foo Barber
favouritefruit: banana
needspeeling: True
seedsremoved: False
otherfamily: ["Rhu Barber", "Harry Barber"]

```



## Phix

Normally I would recommend IupConfig, but the "standard" file format in the task description isn't even close (no [Section] headers, no '=').

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
            li = li[1..k-1] -- (may want upper())
            if find(',',rest) then
                sequence a = split(rest,',')
                for j=1 to length(a) do a[j]=trim(a[j]) end for
                putd(li,a,dini)
            else
                putd(li,rest,dini)
            end if
        else
            putd(li,1,dini) -- ""
        end if
    end if
end for

function visitor(object key, object data, object /*user_data*/)
    ?{key,data}
    return 1
end function
traverse_dict(routine_id("visitor"),0,dini)
?getd("FAVOURITEFRUIT",dini)
```

{{out}}

```txt

{"FAVOURITEFRUIT","banana"}
{"FULLNAME","Foo Barber"}
{"NEEDSPEELING",1}
{"OTHERFAMILY",{"Rhu Barber","Harry Barber"}}
"banana"

```



## PicoLisp


```PicoLisp
(de rdConf (File)
   (in File
      (while (read)
         (set @ (or (pack (clip (line))) T)) ) ) )

(rdConf "conf.txt")
(println FULLNAME FAVOURITEFRUIT NEEDSPEELING SEEDSREMOVED OTHERFAMILY)
(bye)
```

{{out}}

```txt
"Foo Barber" "banana" T NIL "Rhu Barber, Harry Barber"
```



## PL/I


```PL/I

set: procedure options (main);
   declare text character (100) varying;
   declare (fullname, favouritefruit) character (100) varying initial ('');
   declare needspeeling bit (1) static initial ('0'b);
   declare seedsremoved bit (1) static initial ('0'b);
   declare otherfamily(10) character (100) varying;
   declare (i, j) fixed binary;
   declare in file;

   open file (in) title ( '/RD-CON.DAT,TYPE(TEXT),RECSIZE(200)' );

   on endfile (in) go to variables;

   otherfamily = ''; j = 0;

   do forever;
      get file (in) edit (text) (L);
      text = trim(text);

      if length(text) = 0 then iterate;
      if substr(text, 1, 1) = ';' then iterate;
      if substr(text, 1, 1) = '#' then iterate;
      if length(text) >= 9 then
         if substr(text, 1, 9) = 'FULLNAME ' then
            fullname = trim( substr(text, 9) );
      if length(text) >= 15 then
         if substr(text, 1, 15) = 'FAVOURITEFRUIT ' then
            favouritefruit = trim( substr(text, 15) );
      if length(text) >= 12 then
         if text = 'NEEDSPEELING' then needspeeling = '1'b;
      if length(text) >= 12 then
         if text = 'SEEDSREMOVED' then seedsremoved = '1'b;
      if length(text) >= 12 then
       if substr(text, 1, 12) = 'OTHERFAMILY ' then
         do;
                  text = trim(substr(text, 12) );
                  i = index(text, ',');
                  do while (i > 0);
                     j = j + 1;
                     otherfamily(j) = substr(text, 1, i-1);
                     text = trim(substr(text, i+1));
                     i = index(text, ',');
                  end;           
                  j = j + 1;
                  otherfamily(j) = trim(text);
         end;
   end;

variables:
      put skip data (fullname);
      put skip data (favouritefruit);
      put skip data (needspeeling);
      put skip data (seedsremoved);
      do i = 1 to j;
         put skip data (otherfamily(i));
      end;

end set;

```

{{out}} using the given text file as input:-

```txt

FULLNAME='Foo Barber';
FAVOURITEFRUIT='banana';
NEEDSPEELING='1'B;
SEEDSREMOVED='0'B;
OTHERFAMILY(1)='Rhu Barber';
OTHERFAMILY(2)='Harry Barber';

```



## PowerShell


```PowerShell

function Read-ConfigurationFile
{
    [CmdletBinding()]
    Param
    (
        # Path to the configuration file.  Default is "C:\ConfigurationFile.cfg"
        [Parameter(Mandatory=$false, Position=0)]
        [string]
        $Path = "C:\ConfigurationFile.cfg"
    )

    [string]$script:fullName = ""
    [string]$script:favouriteFruit = "" 
    [bool]$script:needsPeeling = $false 
    [bool]$script:seedsRemoved = $false
    [string[]]$script:otherFamily = @()

    function Get-Value ([string]$Line)
    {
        if ($Line -match "=")
        {
            [string]$value = $Line.Split("=",2).Trim()[1]
        }
        elseif ($Line -match " ")
        {
            [string]$value = $Line.Split(" ",2).Trim()[1]
        }

        $value
    }

    # Process each line in file that is not a comment.
    Get-Content $Path | Select-String -Pattern "^[^#;]" | ForEach-Object {

        [string]$line = $_.Line.Trim()

        if ($line -eq [String]::Empty)
        {
            # do nothing for empty lines
        }
        elseif ($line.ToUpper().StartsWith("FULLNAME"))
        {
            $script:fullName = Get-Value $line
        }
        elseif ($line.ToUpper().StartsWith("FAVOURITEFRUIT"))
        {
            $script:favouriteFruit = Get-Value $line
        }
        elseif ($line.ToUpper().StartsWith("NEEDSPEELING"))
        {
            $script:needsPeeling = $true
        }
        elseif ($line.ToUpper().StartsWith("SEEDSREMOVED"))
        {
            $script:seedsRemoved = $true
        }
        elseif ($line.ToUpper().StartsWith("OTHERFAMILY"))
        {
            $script:otherFamily = (Get-Value $line).Split(',').Trim()
        }
    }

    Write-Verbose -Message ("{0,-15}= {1}" -f "FULLNAME", $script:fullName)
    Write-Verbose -Message ("{0,-15}= {1}" -f "FAVOURITEFRUIT", $script:favouriteFruit)
    Write-Verbose -Message ("{0,-15}= {1}" -f "NEEDSPEELING", $script:needsPeeling)
    Write-Verbose -Message ("{0,-15}= {1}" -f "SEEDSREMOVED", $script:seedsRemoved)
    Write-Verbose -Message ("{0,-15}= {1}" -f "OTHERFAMILY", ($script:otherFamily -join ", "))
}

```

I stored the file in ".\temp.txt" and there is no output unless the -Verbose switch is used:

```PowerShell

Read-ConfigurationFile -Path .\temp.txt -Verbose

```

{{Out}}

```txt

VERBOSE: FULLNAME       = Foo Barber
VERBOSE: FAVOURITEFRUIT = banana
VERBOSE: NEEDSPEELING   = True
VERBOSE: SEEDSREMOVED   = False
VERBOSE: OTHERFAMILY    = Rhu Barber, Harry Barber

```

Test if the variables are set:

```PowerShell

Get-Variable -Name fullName, favouriteFruit, needsPeeling, seedsRemoved, otherFamily

```

{{Out}}

```txt

Name                           Value
----                           -----
fullName                       Foo Barber
favouriteFruit                 banana
needsPeeling                   True
seedsRemoved                   False
otherFamily                    {Rhu Barber, Harry Barber}

```


=== Using Switch -Regex ===

```PowerShell

Function Read-ConfigurationFile {
   [CmdletBinding()]
   [OutputType([Collections.Specialized.OrderedDictionary])]
   Param (
   [Parameter(
      Mandatory=$true,
      Position=0
      )
   ]
   [Alias('LiteralPath')]
   [ValidateScript({
      Test-Path -LiteralPath $PSItem -PathType 'Leaf'
      })
   ]
   [String]
   $_LiteralPath
   )

   Begin {
      Function Houdini-Value ([String]$_Text) {
         $__Aux = $_Text.Trim()
         If ($__Aux.Length -eq 0) {
            $__Aux = $true
         } ElseIf ($__Aux.Contains(',')) {
            $__Aux = $__Aux.Split(',') |
               ForEach-Object {
                  If ($PSItem.Trim().Length -ne 0) {
                     $PSItem.Trim()
                  }
               }
         }
         Return $__Aux
      }
   }
   
   Process {
      $__Configuration = [Ordered]@{}
      # Config equivalent pattern
      # Select-String -Pattern '^\s*[^\s;#=]+.*\s*$' -LiteralPath '.\filename.cfg'
      Switch -Regex -File $_LiteralPath {

         '^\s*[;#=]|^\s*$'  {
            Write-Verbose -Message "v$(' '*20)ignored"
            Write-Verbose -Message $Matches[0]
            Continue
         }

         '^([^=]+)=(.*)$' {
            Write-Verbose -Message '↓← ← ← ← ← ← ← ← ← ← equal pattern'
            Write-Verbose -Message $Matches[0]
            $__Name,$__Value = $Matches[1..2]
            $__Configuration[$__Name.Trim()] = Houdini-Value($__Value)
            Continue
         }

         '^\s*([^\s;#=]+)(.*)(\s*)$' {
            Write-Verbose -Message '↓← ← ← ← ← ← ← ← ← ← space or tab pattern or only name'
            Write-Verbose -Message $Matches[0]
            $__Name,$__Value = $Matches[1..2]
            $__Configuration[$__Name.Trim()] = Houdini-Value($__Value)
            Continue
         }

      }
      Return $__Configuration
   }
}

Function Show-Value ([Collections.Specialized.OrderedDictionary]$_Dictionary, $_Index, $_SubIndex) {
   $__Aux = $_Index + ' = '
   If ($_Dictionary[$_Index] -eq $null) {
      $__Aux += $false
   } ElseIf ($_Dictionary[$_Index].Count -gt 1) {
      If ($_SubIndex -eq $null) {
         $__Aux += $_Dictionary[$_Index] -join ','
      } Else {
         $__Aux = $_Index + '(' + $_SubIndex + ') = '
         If ($_Dictionary[$_Index][$_SubIndex] -eq $null) {
            $__Aux += $false		 
         } Else {
            $__Aux += $_Dictionary[$_Index][$_SubIndex]
         }
      }
   } Else {
      $__Aux += $_Dictionary[$_Index]
   }
   Return $__Aux
}

```

Setting variable

```PowerShell

$Configuration = Read-ConfigurationFile -LiteralPath '.\config.cfg'

```


Show variable

```PowerShell

$Configuration

```

{{Out}}

```txt

Name                           Value
----                           -----
FULLNAME                       Foo Barber
FAVOURITEFRUIT                 banana
NEEDSPEELING                   True
OTHERFAMILY                    {Rhu Barber, Harry Barber}

```


Using customize function

```PowerShell

Show-Value $Configuration 'fullname'
Show-Value $Configuration 'favouritefruit'
Show-Value $Configuration 'needspeeling'
Show-Value $Configuration 'seedsremoved'
Show-Value $Configuration 'otherfamily'
Show-Value $Configuration 'otherfamily' 0
Show-Value $Configuration 'otherfamily' 1
Show-Value $Configuration 'otherfamily' 2

```

{{Out}}

```txt

fullname = Foo Barber
favouritefruit = banana
needspeeling = True
seedsremoved = False
otherfamily = Rhu Barber,Harry Barber
otherfamily(0) = Rhu Barber
otherfamily(1) = Harry Barber
otherfamily(2) = False

```


Using index variable

```PowerShell

'$Configuration[''fullname'']'
$Configuration['fullname']
'$Configuration.''fullname'''
$Configuration.'fullname'
'$Configuration.Item(''fullname'')'
$Configuration.Item('fullname')
'$Configuration[0]'
$Configuration[0]
'$Configuration.Item(0)'
$Configuration.Item(0)
' '
'=== $Configuration[''otherfamily''] ==='
$Configuration['otherfamily']
'=== $Configuration[''otherfamily''][0] ==='
$Configuration['otherfamily'][0]
'=== $Configuration[''otherfamily''][1] ==='
$Configuration['otherfamily'][1]
' '
'=== $Configuration.''otherfamily'' ==='
$Configuration.'otherfamily'
'=== $Configuration.''otherfamily''[0] ==='
$Configuration.'otherfamily'[0]
'=== $Configuration.''otherfamily''[1] ==='
$Configuration.'otherfamily'[1]
' '
'=== $Configuration.Item(''otherfamily'') ==='
$Configuration.Item('otherfamily')
'=== $Configuration.Item(''otherfamily'')[0] ==='
$Configuration.Item('otherfamily')[0]
'=== $Configuration.Item(''otherfamily'')[1] ==='
$Configuration.Item('otherfamily')[1]
' '
'=== $Configuration[3] ==='
$Configuration[3]
'=== $Configuration[3][0] ==='
$Configuration[3][0]
'=== $Configuration[3][1] ==='
$Configuration[3][1]
' '
'=== $Configuration.Item(3) ==='
$Configuration.Item(3)
'=== $Configuration.Item(3).Item(0) ==='
$Configuration.Item(3).Item(0)
'=== $Configuration.Item(3).Item(1) ==='
$Configuration.Item(3).Item(1)

```

{{Out}}

```txt

$Configuration['fullname']
Foo Barber
$Configuration.'fullname'
Foo Barber
$Configuration.Item('fullname')
Foo Barber
$Configuration[0]
Foo Barber
$Configuration.Item(0)
Foo Barber

=== $Configuration['otherfamily'] ===
Rhu Barber
Harry Barber
=== $Configuration['otherfamily'][0] ===
Rhu Barber
=== $Configuration['otherfamily'][1] ===
Harry Barber

=== $Configuration.'otherfamily' ===
Rhu Barber
Harry Barber
=== $Configuration.'otherfamily'[0] ===
Rhu Barber
=== $Configuration.'otherfamily'[1] ===
Harry Barber

=== $Configuration.Item('otherfamily') ===
Rhu Barber
Harry Barber
=== $Configuration.Item('otherfamily')[0] ===
Rhu Barber
=== $Configuration.Item('otherfamily')[1] ===
Harry Barber

=== $Configuration[3] ===
Rhu Barber
Harry Barber
=== $Configuration[3][0] ===
Rhu Barber
=== $Configuration[3][1] ===
Harry Barber

=== $Configuration.Item(3) ===
Rhu Barber
Harry Barber
=== $Configuration.Item(3).Item(0) ===
Rhu Barber
=== $Configuration.Item(3).Item(1) ===
Harry Barber

```



## Python

This task is not well-defined, so we have to make some assumptions (see comments in code).

```python
def readconf(fn):
    ret = {}
    with file(fn) as fp:
        for line in fp:
            # Assume whitespace is ignorable
            line = line.strip()
            if not line or line.startswith('#'): continue
            
            boolval = True
            # Assume leading ";" means a false boolean
            if line.startswith(';'):
                # Remove one or more leading semicolons
                line = line.lstrip(';')
                # If more than just one word, not a valid boolean
                if len(line.split()) != 1: continue
                boolval = False
            
            bits = line.split(None, 1)
            if len(bits) == 1:
                # Assume booleans are just one standalone word
                k = bits[0]
                v = boolval
            else:
                # Assume more than one word is a string value
                k, v = bits
            ret[k.lower()] = v
    return ret


if __name__ == '__main__':
    import sys
    conf = readconf(sys.argv[1])
    for k, v in sorted(conf.items()):
        print k, '=', v
```



## Racket


Use the shared [[Racket/Options]] code.


```Racket

#lang racket

(require "options.rkt")

(read-options "options-file")
(define-options fullname favouritefruit needspeeling seedsremoved otherfamily)
(printf "fullname       = ~s\n" fullname)
(printf "favouritefruit = ~s\n" favouritefruit)
(printf "needspeeling   = ~s\n" needspeeling)
(printf "seedsremoved   = ~s\n" seedsremoved)
(printf "otherfamily    = ~s\n" otherfamily)

```


{{out}}

```txt

fullname       = "Foo Barber"
favouritefruit = "banana"
needspeeling   = #t
seedsremoved   = #f
otherfamily    = ("Rhu Barber" "Harry Barber")

```



## RapidQ


```vb

type TSettings extends QObject
    FullName as string
    FavouriteFruit as string
    NeedSpelling as integer
    SeedsRemoved as integer
    OtherFamily as QStringlist
    
    Constructor
        FullName = ""
        FavouriteFruit = ""
        NeedSpelling = 0
        SeedsRemoved = 0
        OtherFamily.clear
    end constructor
end type

Dim Settings as TSettings
dim ConfigList as QStringList
dim x as integer
dim StrLine as string
dim StrPara as string
dim StrData as string

function Trim$(Expr as string) as string
    Result = Rtrim$(Ltrim$(Expr))
end function

Sub ConfigOption(PData as string)
    dim x as integer
    for x = 1 to tally(PData, ",") +1
        Settings.OtherFamily.AddItems Trim$(field$(PData, "," ,x))
    next
end sub 

Function ConfigBoolean(PData as string) as integer
    PData = Trim$(PData)
    Result = iif(lcase$(PData)="true" or PData="1" or PData="", 1, 0)
end function

sub ReadSettings
    ConfigList.LoadFromFile("Rosetta.cfg")
    ConfigList.text = REPLACESUBSTR$(ConfigList.text,"="," ")

    for x = 0 to ConfigList.ItemCount -1
        StrLine = Trim$(ConfigList.item(x))
        StrPara = Trim$(field$(StrLine," ",1))
        StrData = Trim$(lTrim$(StrLine - StrPara))  
    
        Select case UCase$(StrPara)
        case "FULLNAME"       : Settings.FullName = StrData 
        case "FAVOURITEFRUIT" : Settings.FavouriteFruit = StrData 
        case "NEEDSPEELING"   : Settings.NeedSpelling = ConfigBoolean(StrData)
        case "SEEDSREMOVED"   : Settings.SeedsRemoved = ConfigBoolean(StrData)
        case "OTHERFAMILY"    : Call ConfigOption(StrData)
        end select
    next
end sub

Call ReadSettings

```


{{out}}

```txt

Settings.FullName            = Foo Barber
Settings.FavouriteFruit      = banana
Settings.NeedSpelling        = 1
Settings.SeedsRemoved        = 0
Settings.OtherFamily.Item(0) = Rhu Barber
Settings.OtherFamily.Item(1) = Harry Barber

```



## REXX

No assumptions were made about what variables are (or aren't) in the configuration file.

Code was written to make the postmortem report as readable as possible.

```rexx
/*REXX program reads a config (configuration) file and assigns  VARs  as found within.  */
signal on syntax;      signal on novalue         /*handle REXX source program errors.   */
parse arg cFID _ .                               /*cFID:  is the CONFIG file to be read.*/
if cFID==''  then cFID='CONFIG.DAT'              /*Not specified?  Then use the default.*/
bad=                                             /*this will contain all the  bad VARs. */
varList=                                         /*  "    "     "     "   "  good   "   */
maxLenV=0;   blanks=0;   hashes=0;   semics=0;   badVar=0    /*zero all these variables.*/

   do j=0  while lines(cFID)\==0                 /*J:   it counts the lines in the file.*/
   txt=strip(linein(cFID))                       /*read a line (record) from the file,  */
                                                 /*  ··· & strip leading/trailing blanks*/
   if      txt    =''    then do; blanks=blanks+1; iterate; end   /*count # blank lines.*/
   if left(txt,1)=='#'   then do; hashes=hashes+1; iterate; end   /*  "   " lines with #*/
   if left(txt,1)==';'   then do; semics=semics+1; iterate; end   /*  "   "   "     "  ;*/
   eqS=pos('=',txt)                              /*we can't use the   TRANSLATE   BIF.  */
   if eqS\==0  then txt=overlay(' ',txt,eqS)     /*replace the first  '='  with a blank.*/
   parse var txt xxx value;  upper xxx           /*get the variable name and it's value.*/
   value=strip(value)                            /*strip leading and trailing blanks.   */
   if value='' then value='true'                 /*if no value,  then use   "true".     */
   if symbol(xxx)=='BAD'  then do                /*can REXX utilize the variable name ? */
                               badVar=badVar+1;  bad=bad xxx;  iterate  /*append to list*/
                               end
   varList=varList xxx                           /*add it to the list of good variables.*/
   call value xxx,value                          /*now,  use VALUE to set the variable. */
   maxLenV=max(maxLenV,length(value))            /*maxLen of varNames,  pretty display. */
   end   /*j*/

vars=words(varList);          @ig= 'ignored that began with a'
                    say #(j)       'record's(j) "were read from file: " cFID
if blanks\==0  then say #(blanks)  'blank record's(blanks) "were read."
if hashes\==0  then say #(hashes)  'record's(hashes)   @ig   "#  (hash)."
if semics\==0  then say #(semics)  'record's(semics)   @ig   ";  (semicolon)."
if badVar\==0  then say #(badVar)  'bad variable name's(badVar) 'detected:' bad
say;  say 'The list of'    vars    "variable"s(vars)    'and'    s(vars,'their',"it's"),
                                   "value"s(vars)       'follows:'
say;          do k=1  for vars
              v=word(varList,k)
              say  right(v,maxLenV) '=' value(v)
              end   /*k*/
say;  exit                                       /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:       if arg(1)==1  then return arg(3);               return word(arg(2) 's',1)
#:       return right(arg(1),length(j)+11)       /*right justify a number & also indent.*/
err:       do j=1  for arg();  say '***error***    ' arg(j);  say;  end  /*j*/;    exit 13
novalue: syntax:   call err 'REXX program' condition('C') "error",,
         condition('D'),'REXX source statement (line' sigl"):",sourceline(sigl)

```

'''output'''   when using the input (file) as specified in the task description:

```txt

           28 records were read from file:  CONFIG.DAT
            8 blank records were read.
           15 records ignored that began with a # (hash).
            1 record ignored that began with a ; (semicolon).

The list of 4 variables and their values follows:

                FULLNAME = Foo Barber
          FAVOURITEFRUIT = banana
            NEEDSPEELING = true
             OTHERFAMILY = Rhu Barber, Harry Barber

```



## Ruby


```ruby
fullname = favouritefruit = ""
needspeeling = seedsremoved = false
otherfamily = []

IO.foreach("config.file") do |line|
  line.chomp!
  key, value = line.split(nil, 2)
  case key
  when /^([#;]|$)/; # ignore line
  when "FULLNAME"; fullname = value
  when "FAVOURITEFRUIT"; favouritefruit = value
  when "NEEDSPEELING"; needspeeling = true
  when "SEEDSREMOVED"; seedsremoved = true
  when "OTHERFAMILY"; otherfamily = value.split(",").map(&:strip)
  when /^./; puts "#{key}: unknown key"
  end
end

puts "fullname       = #{fullname}"
puts "favouritefruit = #{favouritefruit}"
puts "needspeeling   = #{needspeeling}"
puts "seedsremoved   = #{seedsremoved}"
otherfamily.each_with_index do |name, i|
  puts "otherfamily(#{i+1}) = #{name}"
end
```


{{out}}

```txt

fullname       = Foo Barber
favouritefruit = banana
needspeeling   = true
seedsremoved   = false
otherfamily(1) = Rhu Barber
otherfamily(2) = Harry Barber

```




## Run BASIC


```Runbasic
dim param$(6)
dim paramVal$(6)
param$(1) = "fullname"
param$(2) = "favouritefruit"
param$(3) = "needspeeling"
param$(4) = "seedsremoved"
param$(5) = "otherfamily"
for i = 1 to 6
 paramVal$(i) = "false"
next i

open DefaultDir$ + "\public\a.txt" for binary as #f
while not(eof(#f))
	line input #f, a$
	a$ = trim$(a$)
	if instr("#;",left$(a$,1)) = 0 and a$ <> "" then
		thisParam$ = lower$(word$(a$,1," "))
	  for i = 1 to 5
		if param$(i)    = thisParam$ then
			paramVal$(i)  = "true"
			aa$ = trim$(mid$(a$,len(thisParam$)+2))
			if aa$ <> "" then paramVal$(i) = aa$
		end if
	  next i
	end if
wend
close #f
for i = 1 to 5
  if instr(paramVal$(i),",") > 0 then
   for j = 1 to 2
     print param$(i);"(";j;")";chr$(9);trim$(word$(paramVal$(i),j,","))
   next j
  else
   print param$(i);chr$(9);paramVal$(i)
  end if
next i
```

{{out}}

```txt

fullname	Foo Barber
favouritefruit	banana
needspeeling	true
seedsremoved	false
otherfamily(1)	Rhu Barber
otherfamily(2)	Harry Barber

```



## Rust


```Rust
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::FromIterator;
use std::path::Path;

fn main() {
    let path = String::from("file.conf");
    let cfg = config_from_file(path);
    println!("{:?}", cfg);
}

fn config_from_file(path: String) -> Config {
    let path = Path::new(&path);
    let file = File::open(path).expect("File not found or cannot be opened");
    let content = BufReader::new(&file);
    let mut cfg = Config::new();

    for line in content.lines() {
        let line = line.expect("Could not read the line");
        // Remove whitespaces at the beginning and end
        let line = line.trim();

        // Ignore comments and empty lines
        if line.starts_with("#") || line.starts_with(";") || line.is_empty() {
            continue;
        }

        // Split line into parameter name and rest tokens
        let tokens = Vec::from_iter(line.split_whitespace()); 
        let name = tokens.first().unwrap();
        let tokens = tokens.get(1..).unwrap();

        // Remove the equal signs
        let tokens = tokens.iter().filter(|t| !t.starts_with("="));
        // Remove comment after the parameters
        let tokens = tokens.take_while(|t| !t.starts_with("#") && !t.starts_with(";"));
        
        // Concat back the parameters into one string to split for separated parameters
        let mut parameters = String::new();
        tokens.for_each(|t| { parameters.push_str(t); parameters.push(' '); });
        // Splits the parameters and trims
        let parameters = parameters.split(',').map(|s| s.trim());
        // Converts them from Vec<&str> into Vec<String>
        let parameters: Vec<String> = parameters.map(|s| s.to_string()).collect();
        
        // Setting the config parameters
        match name.to_lowercase().as_str() {
            "fullname" => cfg.full_name = parameters.get(0).cloned(),
            "favouritefruit" => cfg.favourite_fruit = parameters.get(0).cloned(),
            "needspeeling" => cfg.needs_peeling = true,
            "seedsremoved" => cfg.needs_peeling = true,
            "otherfamily" => cfg.other_family = Some(parameters),
            _ => (),
        }
    }

    cfg
}

#[derive(Clone, Debug)]
struct Config {
    full_name: Option<String>,
    favourite_fruit: Option<String>,
    needs_peeling: bool,
    seeds_removed: bool,
    other_family: Option<Vec<String>>,
}

impl Config {
    fn new() -> Config {
        Config {
            full_name: None,
            favourite_fruit: None,
            needs_peeling: false,
            seeds_removed: false,
            other_family: None,
        }
    }
}
```


{{out}}

```txt

Config { full_name: Some("Foo Barber"), favourite_fruit: Some("banana"), needs_peeling: true, seeds_removed: false, other_family: Some(["Rhu Barber", "Harry Barber"]) }

```



## Scala

A "one liner" version which:
* Filters out empty and comment lines
* Splits field name and value(s)
* Adds "true" to value-less fields
* Converts values to (k, List(values)
* Converts the entire collection to a Map


```Scala
val conf = scala.io.Source.fromFile("config.file").
  getLines.
  toList.
  filter(_.trim.size > 0).
  filterNot("#;" contains _(0)).
  map(_ split(" ", 2) toList).
  map(_ :+ "true" take 2).
  map {
    s:List[String] => (s(0).toLowerCase, s(1).split(",").map(_.trim).toList)
  }.toMap
```


Test code:

```Scala

for ((k,v) <- conf) {
  if (v.size == 1)
    println("%s: %s" format (k, v(0)))
  else
    for (i <- 0 until v.size)
      println("%s(%s): %s" format (k, i+1, v(i)))

}

```

{{out}}

```txt

fullname: Foo Barber
favouritefruit: banana
needspeeling: true
otherfamily(1): Rhu Barber
otherfamily(2): Harry Barber

```



## Seed7

The Seed7 library [http://seed7.sourceforge.net/libraries/scanfile.htm scanfile.s7i],
can be used to [http://seed7.sourceforge.net/manual/file.htm#Scanning_a_file scan a file]
with functions like [http://seed7.sourceforge.net/libraries/scanfile.htm#getWord%28inout_file%29 getWord],
[http://seed7.sourceforge.net/libraries/scanfile.htm#skipSpace%28inout_file%29 skipSpace]
and [http://seed7.sourceforge.net/libraries/scanfile.htm#getLine%28inout_file%29 getLine].


```seed7
$ include "seed7_05.s7i";
  include "scanfile.s7i";

var string:  fullname is "";
var string:  favouritefruit is "";
var boolean: needspeeling is FALSE;
var boolean: seedsremoved is FALSE;
var array string: otherfamily is 0 times "";

const proc: main is func
  local
    var file: configFile is STD_NULL;
    var string: symbol is "";
    var integer: index is 0;
  begin
    configFile := open("readcfg.txt", "r");
    configFile.bufferChar := getc(configFile);
    symbol := lower(getWord(configFile));
    while symbol <> "" do
      skipSpace(configFile);
      if symbol = "#" or symbol = ";" then
        skipLine(configFile);
      elsif symbol = "fullname" then
        fullname := getLine(configFile);
      elsif symbol = "favouritefruit" then
        favouritefruit := getLine(configFile);
      elsif symbol = "needspeeling" then
        needspeeling := TRUE;
      elsif symbol = "seedsremoved" then
        seedsremoved := TRUE;
      elsif symbol = "otherfamily" then
        otherfamily := split(getLine(configFile), ",");
        for key index range otherfamily do
          otherfamily[index] := trim(otherfamily[index]);
        end for;
      else
        writeln(" *** Illegal line " <& literal(getLine(configFile)));
      end if;
      symbol := lower(getWord(configFile));
    end while;
    close(configFile);
    writeln("fullname:       " <& fullname);
    writeln("favouritefruit: " <& favouritefruit);
    writeln("needspeeling:   " <& needspeeling);
    writeln("seedsremoved:   " <& seedsremoved);
    for key index range otherfamily do
      writeln(("otherfamily[" <& index <& "]:") rpad 16 <& otherfamily[index]);
    end for;
  end func;
```


{{out}}

```txt

fullname:       Foo Barber
favouritefruit: banana
needspeeling:   TRUE
seedsremoved:   FALSE
otherfamily[1]: Rhu Barber
otherfamily[2]: Harry Barber

```



## Sidef


```ruby
var fullname = (var favouritefruit = "");
var needspeeling = (var seedsremoved = false);
var otherfamily = [];

ARGF.each { |line|
    var(key, value) = line.strip.split(/\h+/, 2)...;

    given(key) {
        when (nil)              { }
        when (/^([#;]|\h*$)/)   { }
        when ("FULLNAME")       { fullname = value }
        when ("FAVOURITEFRUIT") { favouritefruit = value }
        when ("NEEDSPEELING")   { needspeeling = true }
        when ("SEEDSREMOVED")   { seedsremoved = true }
        when ("OTHERFAMILY")    { otherfamily = value.split(',')»strip»() }
        default                 { say "#{key}: unknown key" }
    }
}

say "fullname       = #{fullname}";
say "favouritefruit = #{favouritefruit}";
say "needspeeling   = #{needspeeling}";
say "seedsremoved   = #{seedsremoved}";

otherfamily.each_kv {|i, name|
    say "otherfamily(#{i+1}) = #{name}";
}
```

{{out}}

```txt

fullname       = Foo Barber
favouritefruit = banana
needspeeling   = true
seedsremoved   = false
otherfamily(1) = Rhu Barber
otherfamily(2) = Harry Barber

```



## Smalltalk

{{works with|Smalltalk/X}}
This code retrieves the configuration values as a Dictionary; code to set an object's instance variables follows below:

```smalltalk
dict := Dictionary new.
configFile asFilename readingLinesDo:[:line |
    (line isEmpty or:[ line startsWithAnyOf:#('#' ';') ]) ifFalse:[
        s := line readStream.
        (s skipSeparators; atEnd) ifFalse:[
            |optionName values|
            optionName := s upToSeparator.
            values := (s upToEnd asCollectionOfSubstringsSeparatedBy:$,) 
                         collect:[:each | each withoutSeparators]
                         thenSelect:[:vals | vals notEmpty].
            dict at:optionName asLowercase put:(values isEmpty 
                                                ifTrue:[true] 
                                                ifFalse:[
                                                   values size == 1
                                                     ifTrue:[values first]
                                                     ifFalse:[values]]).    
        ]
    ].
]
```

gives us in dict Dictionary ('fullname'->'Foo Barber' 'needspeeling'->true 'favouritefruit'->'banana' 'otherfamily'->OrderedCollection('Rhu Barber' 'Harry Barber'))

assuming that the target object has setters for each option name, we could write:

```smalltalk
dict keysAndValuesDo:[:eachOption :eachValue | 
   someObject 
       perform:(eachOption,':') asSymbol with:eachValue
       ifNotUnderstood: [ self error: 'unknown option: ', eachOption ]
]
```

or assign variables with:

```smalltalk
fullname := dict at: 'fullname' ifAbsent:false.
needspeeling := dict at: 'needspeeling' ifAbsent:false.
favouritefruit := dict at: 'favouritefruit' ifAbsent:false.
otherfamily := dict at: 'otherfamily' ifAbsent:false.
seedsremoved := dict at: 'seedsremoved' ifAbsent:false.

```



## Tcl

This code stores the configuration values in a global array (<code>cfg</code>) so they can't pollute the global namespace in unexpected ways.

```tcl
proc readConfig {filename {defaults {}}} {
    global cfg
    # Read the file in
    set f [open $filename]
    set contents [read $f]
    close $f
    # Set up the defaults, if supplied
    foreach {var defaultValue} $defaults {
	set cfg($var) $defaultValue
    }
    # Parse the file's contents
    foreach line [split $contents "\n"] {
	set line [string trim $line]
	# Skip comments
	if {[string match "#*" $line] || [string match ";*" $line]} continue
	# Skip blanks
	if {$line eq ""} continue

	if {[regexp {^\w+$} $line]} {
	    # Boolean case
	    set cfg([string tolower $line]) true
	} elseif {[regexp {^(\w+)\s+([^,]+)$} $line -> var value]} {
	    # Simple value case
	    set cfg([string tolower $var]) $value
	} elseif {[regexp {^(\w+)\s+(.+)$} $line -> var listValue]} {
	    # List value case
	    set cfg([string tolower $var]) {}
	    foreach value [split $listValue ","] {
		lappend cfg([string tolower $var]) [string trim $value]
	    }
	} else {
	    error "malformatted config file: $filename"
	}
    }
}

# Need to supply some default values due to config file grammar ambiguities
readConfig "fruit.cfg" {
    needspeeling false
    seedsremoved false
}
puts "Full name: $cfg(fullname)"
puts "Favourite: $cfg(favouritefruit)"
puts "Peeling?   $cfg(needspeeling)"
puts "Unseeded?  $cfg(seedsremoved)"
puts "Family:    $cfg(otherfamily)"
```



## TXR

Prove the logic by transliterating to a different syntax:

```txr
@(collect)
@  (cases)
#@/.*/
@  (or)
;@/.*/
@  (or)
@{IDENT /[A-Z_][A-Z_0-9]+/}@/ */
@(bind VAL ("true"))
@  (or)
@{IDENT /[A-Z_][A-Z_0-9]+/}@(coll)@/ */@{VAL /[^,]+/}@/ */@(end)
@  (or)
@{IDENT /[A-Z_][A-Z_0-9]+/}@(coll)@/ */@{VAL /[^, ]+/}@/,? */@(end)
@(flatten VAL)
@  (or)
@/ */
@  (or)
@  (throw error "bad configuration syntax")
@  (end)
@(end)
@(output)
@  (repeat)
@IDENT = @(rep)@VAL, @(first){ @VAL, @(last)@VAL };@(single)@VAL;@(end)
@  (end)
@(end)

```


Sample run:


```txt
$ txr  configfile.txr  configfile
FULLNAME = Foo Barber;
FAVOURITEFRUIT = banana;
NEEDSPEELING = true;
OTHERFAMILY = { Rhu Barber, Harry Barber };

```



## UNIX Shell

No effort is made to make an exception for SEEDSREMOVED: lines beginning with a semi-colon are treated as comments.
No expectations are made about what variables will be seen in the config file.

{{works with|bash}}

```bash
readconfig() (
    # redirect stdin to read from the given filename
    exec 0<"$1"

    declare -l varname
    while IFS=$' =\t' read -ra words; do
        # is it a comment or blank line?
        if [[ ${#words[@]} -eq 0 || ${words[0]} == ["#;"]* ]]; then
            continue
        fi

        # get the variable name
        varname=${words[0]}

        # split all the other words by comma
        value="${words[*]:1}"
        oldIFS=$IFS IFS=,
        values=( $value )
        IFS=$oldIFS

        # assign the other words to a "scalar" variable or array
        case ${#values[@]} in
            0) printf '%s=true\n' "$varname" ;;
            1) printf '%s=%q\n' "$varname" "${values[0]}" ;;
            *) n=0
               for value in "${values[@]}"; do
                   value=${value# }
                   printf '%s[%d]=%q\n' "$varname" $((n++)) "${value% }"
               done
               ;;
        esac
    done
)

# parse the config file and evaluate the output in the current shell
source <( readconfig config.file )

echo "fullname = $fullname"
echo "favouritefruit = $favouritefruit"
echo "needspeeling = $needspeeling"
echo "seedsremoved = $seedsremoved"
for i in "${!otherfamily[@]}"; do
    echo "otherfamily[$i] = ${otherfamily[i]}"
done
```


{{out}}

```txt
fullname = Foo Barber
favouritefruit = banana
needspeeling = true
seedsremoved = 
otherfamily[0] = Rhu Barber
otherfamily[1] = Harry Barber
```



## VBScript


```vb

Set ofso = CreateObject("Scripting.FileSystemObject")
Set config = ofso.OpenTextFile(ofso.GetParentFolderName(WScript.ScriptFullName)&"\config.txt",1)

config_out = ""

Do Until config.AtEndOfStream
	line = config.ReadLine
	If Left(line,1) <> "#" And Len(line) <> 0 Then
		config_out = config_out & parse_var(line) & vbCrLf
	End If
Loop

WScript.Echo config_out

Function parse_var(s)
	'boolean false
	If InStr(s,";") Then
		parse_var = Mid(s,InStr(1,s,";")+2,Len(s)-InStr(1,s,";")+2) & " = FALSE"
	'boolean true
	ElseIf UBound(Split(s," ")) = 0 Then
		parse_var = s & " = TRUE"
	'multiple parameters
	ElseIf InStr(s,",") Then
		var = Left(s,InStr(1,s," ")-1)
		params = Split(Mid(s,InStr(1,s," ")+1,Len(s)-InStr(1,s," ")+1),",")
		n = 1 : tmp = ""
		For i = 0 To UBound(params)
			parse_var = parse_var & var & "(" & n & ") = " & LTrim(params(i)) & vbCrLf
			n = n + 1
		Next
	'single var and paramater
	Else
		parse_var = Left(s,InStr(1,s," ")-1) & " = " & Mid(s,InStr(1,s," ")+1,Len(s)-InStr(1,s," ")+1)
	End If
End Function

config.Close
Set ofso = Nothing

```


{{Out}}

```txt

FULLNAME = Foo Barber
FAVOURITEFRUIT = banana
NEEDSPEELING = TRUE
SEEDSREMOVED = FALSE
OTHERFAMILY(1) = Rhu Barber
OTHERFAMILY(2) = Harry Barber

```



## Vedit macro language



```vedit
#11 = 0                 // needspeeling = FALSE
#12 = 0                 // seedsremoved = FALSE
Reg_Empty(21)           // fullname
Reg_Empty(22)           // favouritefruit
Reg_Empty(23)           // otherfamily

File_Open("|(PATH_ONLY)\example.cfg")

if (Search("|<FULLNAME|s", BEGIN+ADVANCE+NOERR)) {
    Match("=", ADVANCE)         // skip optional '='
    Reg_Copy_Block(21, CP, EOL_pos)
}
if (Search("|<FAVOURITEFRUIT|s", BEGIN+ADVANCE+NOERR)) {
    Match("=", ADVANCE)
    Reg_Copy_Block(22, CP, EOL_pos)
}
if (Search("|<OTHERFAMILY|s", BEGIN+ADVANCE+NOERR)) {
    Match("=", ADVANCE)
    Reg_Copy_Block(23, CP, EOL_pos)
}
if (Search("|<NEEDSPEELING|s", BEGIN+ADVANCE+NOERR)) {
    #11 = 1
}
if (Search("|<SEEDSREMOVED|s", BEGIN+ADVANCE+NOERR)) {
    #12 = 1
}

Buf_Quit(OK)            // close .cfg file

// Display the variables
Message("needspeeling   = ") Num_Type(#11, LEFT)
Message("seedsremoved   = ") Num_Type(#12, LEFT)
Message("fullname       = ") Reg_Type(21) TN
Message("favouritefruit = ") Reg_Type(22) TN
Message("otherfamily    = ") Reg_Type(23) TN
```


{{out}}

```txt

needspeeling   = 1
seedsremoved   = 0
fullname       = Foo Barber
favouritefruit = banana
otherfamily    = Rhu Barber, Harry Barber

```



## Visual Basic



```vb
' Configuration file parser routines.
'
' (c) Copyright 1993 - 2011 Mark Hobley
'
' This configuration parser contains code ported from an application program
' written in Microsoft Quickbasic
'
' This code can be redistributed or modified under the terms of version 1.2 of
' the GNU Free Documentation Licence as published by the Free Software Foundation.

Sub readini()
  var.filename = btrim$(var.winpath) & ini.inifile
  var.filebuffersize = ini.inimaxlinelength
  Call openfileread
  If flg.error = "Y" Then
    flg.abort = "Y"
    Exit Sub
  End If
  If flg.exists <> "Y" Then
    flg.abort = "Y"
    Exit Sub
  End If
  var.inistream = var.stream
readinilabela:
  Call readlinefromfile
  If flg.error = "Y" Then
    flg.abort = "Y"
    Call closestream
    flg.error = "Y"
    Exit Sub
  End If
  If flg.endoffile <> "Y" Then
    iniline$ = message$
    If iniline$ <> "" Then
      If Left$(iniline$, 1) <> ini.commentchar AND Left$(iniline$, 1) <> ini.ignorechar Then
        endofinicommand% = 0
        For l% = 1 To Len(iniline$)
          If Mid$(iniline$, l%, 1) < " " Then
            endofinicommand% = l%
          End If
          If Not (endofinicommand%) Then
            If Mid$(iniline$, l%, 1) = " " Then
              endofinicommand% = l%
            End If
          End If
          If endofinicommand% Then
            l% = Len(iniline$)
          End If
        Next l%
        iniarg$ = ""
        If endofinicommand% Then
          If endofinicommand% <> Len(iniline$) Then
            iniarg$ = btrim$(Mid$(iniline$, endofinicommand% + 1))
            If iniarg$ = "" Then
              GoTo readinilabelb
            End If
            inicommand$ = Left$(iniline$, endofinicommand% - 1)
          End If
        Else
          inicommand$ = btrim$(iniline$)
        End If
readinilabelb:
        'interpret command
        inicommand$ = UCase$(inicommand$)
        Select Case inicommand$
          Case "FULLNAME"
            If iniarg$ <> "" Then
              ini.fullname = iniarg$
            End If
          Case "FAVOURITEFRUIT"
            If iniarg$ <> "" Then
              ini.favouritefruit = iniarg$
            End If
          Case "NEEDSPEELING"
            ini.needspeeling = "Y"
          Case "SEEDSREMOVED"
            ini.seedsremoved = "Y"
          Case "OTHERFAMILY"
            If iniarg$ <> "" Then
              ini.otherfamily = iniarg$
              CALL familyparser
            End If
          Case Else
            '!! error handling required
        End Select
      End If
    End If
    GoTo readinilabela
  End If
  Call closestream
  Exit Sub
readinierror:

End Sub

Sub openfileread()
  flg.streamopen = "N"
  Call checkfileexists
  If flg.error = "Y" Then Exit Sub
  If flg.exists <> "Y" Then Exit Sub
  Call getfreestream
  If flg.error = "Y" Then Exit Sub
  var.errorsection = "Opening File"
  var.errordevice = var.filename
  If ini.errortrap = "Y" Then
    On Local Error GoTo openfilereaderror
  End If
  flg.endoffile = "N"
  Open var.filename For Input As #var.stream Len = var.filebuffersize
  flg.streamopen = "Y"
  Exit Sub
openfilereaderror:
  var.errorcode = Err
  Call errorhandler
  resume '!!
End Sub

Public Sub checkfileexists()
  var.errorsection = "Checking File Exists"
  var.errordevice = var.filename
  If ini.errortrap = "Y" Then
    On Local Error GoTo checkfileexistserror
  End If
  flg.exists = "N"
  If Dir$(var.filename, 0) <> "" Then
    flg.exists = "Y"
  End If
  Exit Sub
checkfileexistserror:
  var.errorcode = Err
  Call errorhandler
End Sub

Public Sub getfreestream()
  var.errorsection = "Opening Free Data Stream"
  var.errordevice = ""
  If ini.errortrap = "Y" Then
    On Local Error GoTo getfreestreamerror
  End If
  var.stream = FreeFile
  Exit Sub
getfreestreamerror:
  var.errorcode = Err
  Call errorhandler
  resume '!!
End Sub

Sub closestream()
  If ini.errortrap = "Y" Then
    On Local Error GoTo closestreamerror
  End If
  var.errorsection = "Closing Stream"
  var.errordevice = ""
  flg.resumenext = "Y"
  Close #var.stream
  If flg.error = "Y" Then
    flg.error = "N"
    '!! Call unexpectederror
  End If
  flg.streamopen = "N"
  Exit Sub
closestreamerror:
  var.errorcode = Err
  Call errorhandler
  resume next
End Sub

Sub readlinefromfile()
  If ini.errortrap = "Y" Then
    On Local Error GoTo readlinefromfileerror
  End If
  If EOF(var.stream) Then
    flg.endoffile = "Y"
    Exit Sub
  End If
  Line Input #var.stream, tmp$
  message$ = tmp$
  Exit Sub
readlinefromfileerror:
  var.errorcode = Err
  Call errorhandler
  resume '!!
End Sub

Public Sub errorhandler()
  tmp$ = btrim$(var.errorsection)
  tmp2$ = btrim$(var.errordevice)
  If tmp2$ <> "" Then
    tmp$ = tmp$ + " (" + tmp2$ + ")"
  End If
  tmp$ = tmp$ + " : " + Str$(var.errorcode)
  tmp1% = MsgBox(tmp$, 0, "Error!")
  flg.error = "Y"
  If flg.resumenext = "Y" Then
    flg.resumenext = "N"
'    Resume Next
  Else
    flg.error = "N"
'    Resume
  End If
End Sub

Public Function btrim$(arg$)
  btrim$ = LTrim$(RTrim$(arg$))
End Function
```



## Yabasic


```Yabasic
a = open("rosetta_read.cfg")

while(not eof(#a))
    FLAG = true : REMARK = false
    line input #a line$
    line$ = trim$(line$)
    ll = len(line$)
    c$ = left$(line$, 1)
    switch(c$)
        case "": case "#": REMARK = true : break
        case ";": FLAG = false : line$ = trim$(right$(line$, ll - 1)) : break
        default: MULTI = instr(line$, ",")
    end switch  

    if not REMARK then
        GAP = instr(line$, "=") : if not GAP GAP = instr(line$, " ")
        if not GAP then
            print line$, " = ";
            if FLAG then print "true" else print "false" end if
        else
            if MULTI then
                count = 1 : SG = GAP
                repeat
                    print left$(line$, GAP - 1), "(", count, ") = ", trim$(mid$(line$, SG + 1, MULTI - SG - 1))
                    count = count + 1
                    SG = MULTI + 1 : MULTI = instr(line$, ",", SG)
                    if not MULTI MULTI = ll + 1
                until(SG > ll)
            else
                print left$(line$, GAP - 1), " = ", trim$(right$(line$, ll - GAP))
            end if
        end if
    end if
wend

close #a
```



## zkl

First, a simple parser that knows nothing about contents, just format.

```zkl
fcn readConfigFile(config){  //--> read only dictionary
   conf:=Dictionary();
   foreach line in (config){
      line=line.strip();
      if (not line or "#"==line[0] or ";"==line[0]) continue;
      line=line.replace("\t"," ");
      n:=line.find(" ");
      if (Void==n) conf[line.toLower()]=True;  // eg NEEDSPEELING
      else{
	 key:=line[0,n].toLower(); line=line[n,*];
	 n=line.find(",");
	 if (Void!=n) conf[key]=line.split(",").apply("strip").filter();
	 else conf[key]=line;
      }
   }
   conf.makeReadOnly();
}

conf:=readConfigFile(File("foo.conf"));
```

Which may be good enough; query the hash table to get a set option or a default if it wasn't set:

```zkl
foreach k,v in (conf){ println(k," : ",v) }
println("Value of seedsremoved = ",conf.find("seedsremoved",False));
```

{{out}}

```txt

needspeeling : True
otherfamily : L("Rhu Barber","Harry Barber")
favouritefruit :  banana
fullname :  Foo Barber
Value of seedsremoved = False

```

If your program actually wants to use the options as variables, the following sets variables to values in the config file, ignoring misspellings, or values you don't care about. You are not allowed to create variables "on the fly".

```zkl
var needspeeling,otherfamily,favouritefruit,fullname,seedsremoved;
foreach k,v in (conf){ try{ setVar(k,v) }catch{} };
foreach k,v in (vars){ println(k," : ",v) }
println("Value of seedsremoved = ",seedsremoved);
```

{{out}}

```txt

favouritefruit :  banana
fullname :  Foo Barber
needspeeling : True
otherfamily : L("Rhu Barber","Harry Barber")
seedsremoved : Void
Value of seedsremoved = Void

```



{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}

[[Category:Initialization]]
