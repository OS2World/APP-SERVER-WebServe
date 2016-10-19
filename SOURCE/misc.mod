(**************************************************************************)
(*                                                                        *)
(*  Web server session manager                                            *)
(*  Copyright (C) 2015   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE Misc;

        (********************************************************)
        (*                                                      *)
        (*           Miscellaneous support procedures           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 May 2015                      *)
        (*  Last edited:        2 May 2015                      *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  Exists, OpenNewFile;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

(************************************************************************)

CONST
    Nul = CHR(0);  Space = ' ';

VAR
    (* String used in creating a unique file name. *)

    NextName: ARRAY [0..7] OF CHAR;
    NextNameLock: Lock;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

PROCEDURE MakeUniqueName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                NextName[N] := '0';
                IF N > 0 THEN
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Obtain (NextNameLock);
        Strings.Assign (NextName, name);
        Increment (7);
        Release (NextNameLock);
    END MakeUniqueName;

(************************************************************************)

PROCEDURE MakeNewFilename (VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form xxxxxxxx.tmp, where xxxxxxxx is  *)
    (* chosen such that a file of that name does not already exist.     *)

    BEGIN
        REPEAT
            MakeUniqueName (NewName);
            Strings.Append (".tmp", NewName);
        UNTIL NOT Exists(NewName);
    END MakeNewFilename;

(************************************************************************)

PROCEDURE OpenTemporaryFile (VAR (*OUT*) name: ARRAY OF CHAR): ChanId;

    (* Opens a new file with name xxxxxxxx.tmp, where xxxxxxxx is       *)
    (* chosen to avoid filename clashes.                                *)

    VAR cid: ChanId;

    BEGIN
        REPEAT
            MakeNewFilename (name);
            cid := OpenNewFile (name, FALSE);
        UNTIL cid <> NoSuchChannel;
        RETURN cid;
    END OpenTemporaryFile;

(************************************************************************)
(*                            STRING MATCHING                           *)
(************************************************************************)

PROCEDURE SkipLeadingSpaces (VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* Deletes any leading space characters in buffer. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        WHILE (j <= HIGH(buffer)) AND (buffer[j] = Space) DO
            INC (j);
        END (*WHILE*);
        IF j > 0 THEN
            Strings.Delete (buffer, 0, j);
        END (*IF*);
    END SkipLeadingSpaces;

(************************************************************************)

PROCEDURE MatchLeading (VAR (*INOUT*) buffer: ARRAY OF CHAR;
                                            head: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff head is a leading substring of buffer, modulo   *)
    (* alphabetic case.  If we have a match, strips out head from       *)
    (* buffer, as well as any following space characters.               *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j > HIGH(head)) OR (j > HIGH(buffer)) OR (head[j] = Nul) THEN
                WHILE (j <= HIGH(buffer)) AND (buffer[j] = Space) DO
                    INC (j);
                END (*WHILE*);
                Strings.Delete (buffer, 0, j);
                RETURN TRUE;
            ELSIF CAP(buffer[j]) <> CAP(head[j]) THEN
                RETURN FALSE;
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);
    END MatchLeading;

(************************************************************************)
(*                         MODULE INITIALISATION                        *)
(************************************************************************)

BEGIN
    NextName := "00000000";
    CreateLock (NextNameLock);
FINALLY
    DestroyLock (NextNameLock);
END Misc.

