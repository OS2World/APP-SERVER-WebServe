/*----------------------------------------------------------
   Appends a build level to WEBSERVE.EXE and SETUP.EXE.

           Author:       Peter Moylan
           Last revised: 27 January 2018

   Usage:
           bldlvl ver

           where ver is the version string

------------------------------------------------------------*/

parse arg ver
projHost = "PJM2"
timestamp = LEFT(DATE() TIME(),25)LEFT(projHost,10)
signature0 = "@#Peter Moylan:"ver"#@##1## "timestamp"::EN:AU:::@@"
outfile = "level.txt"
"DEL "outfile" 2> nul"
CALL LINEOUT outfile, signature0||"WebServe light-weight web server for OS/2, eCS, ArcaOS"
CALL STREAM outfile,'C','CLOSE'
"@copy webserve.exe /B + level.txt webserve.exe /B >nul"

"@DEL "outfile
CALL LINEOUT outfile, signature0||"Setup program for WebServe"
CALL STREAM outfile,'C','CLOSE'
"@copy setup.exe /B + level.txt setup.exe /B >nul"
"@DEL "outfile

exit

