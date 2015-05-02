/* Batch file to create the WebServe distribution. */

'del webserve*.zip 2>nul'
'del temp /N 2>nul'
'call deltree /Y temp >nul'           /* deltree3.zip from Hobbes */
'xc =p webserve.prj'
'xc =p setup.prj'
'\apps\lxlite\lxlite *.exe'

/* Generate symbol files.  The next four lines can be skipped   */
/* if you don't have Perl.                                      */

'copy C:\os2tk45\bin\mapxqs.exe'
'call PerlEnv.cmd'
perl 'D:\Apps\scripts\makexqs.pl' webserve.map
say "webserve.sym and webserve.xqs should now exist"

/* Build level. */

/* call seticon */
say "Calling version()"
ver = version()
say "Returned from version()"
call bldlvl ver

/* Zip up the source files. */

'del src.zip 2>nul'
'Imports WebServe | zip -q -j -u src.zip -@'
'Imports Setup | zip -q -j -u src.zip -@'
'zip -q src.zip WebServe.prj Setup.prj xc.red'

/* Copy the files we want to zip up into a "temp" directory. */

mkdir temp
cd temp
'copy D:\Dev1\General\doc\gpl.txt'
mkdir SOURCE
'move ..\src.zip SOURCE'
cd SOURCE
'unzip -q -o src.zip'
del src.zip
'cd ..'
mkdir doc
'copy ..\doc\changes.doc doc'
'copy ..\README'
'copy ..\file_id.diz'
'copy ..\WebServe.exe'
'copy ..\WebServe.sym'
'copy ..\WebServe.xqs'
'copy ..\WebServe.map'
'copy ..\Setup.exe'
'copy ..\setup.*.lng'
'copy ..\makezip.cmd'
'copy ..\version.cmd'
'copy ..\bldlvl.cmd'

/* Create the final zip file. */

'zip -q -r ..\WebServe'ver'.zip .'

/* Remove temporary files and directories. */

'del SOURCE\* /n'
rmdir SOURCE
'del doc\* /n'
rmdir doc
'del * /n'
'cd ..'
rmdir temp

