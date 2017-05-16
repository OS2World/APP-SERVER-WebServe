/* Script to move the log files, with renaming, to the 'logs' subdirectory. */
/* You should arrange to run this regularly, using any of the               */
/* cron-like utilities for OS/2.  My own approach is to create a            */
/* program object for this file, and use the 'schedule' feature of          */
/* DragText to run it every day.                                            */

CALL MoveFile 'webserve.log'
CALL MoveFile 'common.log'
EXIT

/****************************************************************/
/*                     MOVE ONE LOG FILE                        */
/****************************************************************/

MoveFile: PROCEDURE

    /* Moves one file, with renaming, to the directory 'logs'.  */

    PARSE ARG srcfile
    IF STREAM(srcfile, 'C', 'QUERY EXISTS') \= '' THEN DO
        prefix = DATE('S')||'.'
        dstfile = prefix||srcfile

        /* Check for name duplication. */

        IF STREAM('logs\'dstfile, 'C', 'QUERY EXISTS') \= '' THEN DO
            prefix = DATE('S')'.'TIME('S')||'.'
            dstfile = prefix||srcfile
        END /* DO */

        '@RENAME 'srcfile' 'dstfile
        '@MOVE 'dstfile 'logs >nul'
    END /* DO */

    RETURN

