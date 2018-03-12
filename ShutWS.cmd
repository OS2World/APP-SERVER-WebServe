/****************************************************************/
/*          Signals a global event semaphore to tell            */
/*                   WebServe to shut down.                     */
/*                                                              */
/*       Programmer:      P. Moylan                             */
/*       Last modified:   10 March 2018                         */
/*                                                              */
/* Prerequisite: for this to work you must have RXU.DLL in      */
/* your LIBPATH.  RXU, a Rexx library written by Dave Boll, is  */
/* available from Hobbes with the name rxu1a.zip.               */
/*                                                              */
/* If you invoke the script with a parameter 'Q', it signals    */
/* twice on the semaphore, to abort any sessions in progress.   */
/****************************************************************/

parse upper arg ourarg
call rxfuncadd 'rxuinit','rxu','rxuinit'
call rxuinit
SemName = "\SEM32\WEBSERVE\SHUTDOWN"
if RxOpenEventSem(hev, SemName) \= 0 then
    rc = RxCreateEventSem( hev ,'Shared', SemName, 'Reset')
call RxPostEventSem hev
say "First post of SHUTDOWN semaphore"
if ourarg = 'Q' then
    do
       call RxResetEventSem hev
       call SysSleep 1
       call RxPostEventSem hev
       say "Second post of SHUTDOWN semaphore"
    end
call RxResetEventSem hev
call RxCloseEventSem hev
say "Waiting for WebServe to finish shutting down"
SemName = "\SEM32\WEBSERVE\FINISHED"
if RxOpenEventSem(hev, SemName) \= 0 then
    rc = RxCreateEventSem( hev ,'Shared', SemName, 'Reset')
call RxWaitEventSem hev
call RxResetEventSem hev
call RxCloseEventSem hev
call RxuTerm
say "Shutdown complete"

exit

