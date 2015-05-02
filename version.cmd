/*----------------------------------------------------------
   Returns the version number of WebServe.

           Author:       Peter Moylan
           Last revised: 25 April 2015

   Usage:
           ver = version()

           (Run this from the WebServe top-level directory)

------------------------------------------------------------*/

DEFFile = "DEF\WSV.def"

DO FOREVER
    IF lines(DEFFile) != 1 THEN LEAVE
    parse value linein(DEFFile) with kwd'='val
    kwd = STRIP(kwd)
    IF kwd = "version" THEN LEAVE
END

/* Extra the part of val inside double quotes. */

PARSE VALUE val WITH v1 '"' version '"' v2
RETURN version

exit

