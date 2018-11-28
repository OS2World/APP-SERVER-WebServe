(**************************************************************************)
(*                                                                        *)
(*  Web server                                                            *)
(*  Copyright (C) 2018   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE SSI;

        (********************************************************)
        (*                                                      *)
        (*                 Server-side include                  *)
        (*                                                      *)
        (*  The purpose of this module is to handle embedded    *)
        (*  SSI directives inside an shtml file.                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 May 2015                      *)
        (*  Last edited:        11 November 2018                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*  Directives handled so far:                          *)
        (*      #include                                        *)
        (*      #exec                                           *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Arith64 IMPORT
    (* type *)  CARD64;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenOldFile, CloseFile, ReadLine, FWriteString, FWriteLn,
                DeleteFile, MoveFile;

FROM Misc IMPORT
    (* proc *)  MakeNewFilename, OpenTemporaryFile, MatchLeading;

FROM Requests IMPORT
    (* type *)  Session,
    (* proc *)  ExecProgram;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  LocateFile;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    DirInfoPtr = POINTER TO
                    RECORD
                        domain: Domain;
                        CurrentDir: FilenameString;
                    END (*RECORD*);

(************************************************************************)
(*                   PROCESSING A SINGLE SSI DIRECTIVE                  *)
(************************************************************************)

PROCEDURE GetFilename (virtual, cmd: BOOLEAN;
                        DirInfo: DirInfoPtr;
                        VAR (*INOUT*) line: ARRAY OF CHAR;
                        VAR (*OUT*) name: ARRAY OF CHAR): BOOLEAN;

    (* Decodes a file name in quotes.  If virtual is TRUE, the name is  *)
    (* relative to the domain root, otherwise it is relative to the     *)
    (* current directory.  In the latter case, absolute paths are       *)
    (* forbidden.  We also forbid ".." anywhere in the specification.   *)
    (* On exit, line holds what has not been processed.                 *)
    (* If cmd=TRUE, we are instead looking for a command that does not  *)
    (* need to be relative to any root directory.                       *)

    VAR size: CARD64;  pos: CARDINAL;
        quotechar: ARRAY [0..0] OF CHAR;
        found, leadingslash, SHTML: BOOLEAN;
        filespec, temp: FilenameString;

    BEGIN
        (* Strip the quote characters. *)

        quotechar[0] := line[0];
        IF (quotechar[0] =  '"') OR (quotechar[0] = "'") THEN
            Strings.Delete (line, 0, 1);
        ELSE
            quotechar[0] := ' ';
        END (*IF*);
        Strings.FindNext (quotechar, line, 0, found, pos);
        IF NOT found THEN
            pos := LENGTH(line);
        END (*IF*);
        Strings.Assign (line, filespec);
        IF filespec[pos] <> Nul THEN
            filespec[pos] := Nul;
            INC (pos);
        END (*IF*);
        Strings.Delete (line, 0, pos+1);

        (* In the "cmd" case, no more processing is needed. *)

        IF cmd THEN
            Strings.Assign (filespec, name);
            RETURN TRUE;
        END (*IF*);

        (* Now we have the file specification in filespec.  Check       *)
        (* it for legality, and convert it to a specification relative  *)
        (* to the domain root.                                          *)

        leadingslash := (filespec[0] = '\') OR (filespec[0] = '/');
        IF leadingslash THEN
            filespec[0] := '/';
        END (*IF*);
        IF virtual THEN
            IF NOT leadingslash THEN
                Strings.Insert ('/', 0, filespec);
            END (*IF*);
        ELSIF leadingslash THEN
            RETURN FALSE;
        ELSE
            Strings.Assign (DirInfo^.CurrentDir, temp);
            Strings.Append (filespec, temp);
            Strings.Assign (temp, filespec);
        END (*IF*);
        Strings.FindNext ("..", filespec, 0, found, pos);
        IF found THEN
            RETURN FALSE;
        END (*IF*);

        (* At last, we have a specification relative to this domain. *)

        RETURN LocateFile (DirInfo^.domain, filespec, name, temp, size, found, SHTML);

    END GetFilename;

(************************************************************************)

PROCEDURE DoSSIProcess (sess: Session;  outcid: ChanId;
                          DirInfo: DirInfoPtr;
                            VAR (*INOUT*) filename: ARRAY OF CHAR);
                                                                  FORWARD;

(************************************************************************)

PROCEDURE HandleDirective (sess: Session;  incid, outcid: ChanId;
                            DirInfo: DirInfoPtr;
                            VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Handles the directive at the beginning of line, returns with     *)
    (* line holding whatever has not been processed.  If it's a         *)
    (* directive that we can't handle, we copy that directive to the    *)
    (* output file, including the leading "<!--#" that the caller has   *)
    (* deleted.                                                         *)

    VAR handled, found, virtual, cmd: BOOLEAN;
        pos: CARDINAL;
        partline: ARRAY [0..1023] OF CHAR;
        langstr: ARRAY [0..31] OF CHAR;
        tempfile: FilenameString;

    BEGIN
        handled := FALSE;  virtual := FALSE;  cmd := FALSE;
        Strings.Assign (line, partline);
        IF MatchLeading (line, "include") THEN

            (* The #include directive. *)

            IF MatchLeading (line, "virtual=") THEN
                virtual := TRUE;  handled := TRUE;
            ELSIF MatchLeading (line, "file=") THEN
                handled := TRUE;
            ELSE
                Strings.Assign (partline, line);
            END (*IF*);
            IF handled THEN
                IF GetFilename (virtual, FALSE, DirInfo, line, partline) THEN
                    DoSSIProcess (sess, outcid, DirInfo, partline);
                END (*IF*);
            END (*IF*);

        ELSIF MatchLeading (line, "exec") THEN

            (* The #include directive. *)

            IF MatchLeading (line, "cmd=") THEN
                cmd := TRUE;  handled := TRUE;
            ELSIF MatchLeading (line, "cgi=") THEN
                handled := TRUE;
            ELSE
                Strings.Assign (partline, line);
            END (*IF*);
            IF handled THEN
                IF GetFilename (TRUE, cmd, DirInfo, line, partline) THEN
                    MakeNewFilename (tempfile);
                    langstr := "en";
                    IF NOT cmd THEN
                        (* For now, disable the "cmd" option because    *)
                        (* of security concerns.                        *)
                        ExecProgram (sess, partline, tempfile);
                        DoSSIProcess (sess, outcid, DirInfo, tempfile);
                    END (*IF*);
                    DeleteFile (tempfile);
                END (*IF*);
            END (*IF*);

        END (*IF*);

        IF NOT handled THEN
            FWriteString (outcid, "<!--#");
        END (*IF*);

        (* Search for end of comment.  We discard the comment if we     *)
        (* have handled the directive, and copy it over if we haven't.  *)

        found := FALSE;
        WHILE NOT found DO
            Strings.FindNext ("-->", line, 0, found, pos);
            IF found THEN
                Strings.Assign (line, partline);
                INC (pos, 3);
                partline[pos] := Nul;
                Strings.Delete (line, 0, pos);
                IF NOT handled THEN
                    FWriteString (outcid, partline);
                END (*IF*);
            ELSE
                (* Keep searching for the end of the comment. *)
                IF NOT handled THEN
                    FWriteString (outcid, line);
                    FWriteLn (outcid);
                END (*IF*);
                ReadLine (incid, line);
            END (*IF*);
        END (*IF*);

    END HandleDirective;

(************************************************************************)
(*                          PROCESSING ONE FILE                         *)
(************************************************************************)

PROCEDURE DoSSIProcess (sess: Session;  outcid: ChanId;
                            DirInfo: DirInfoPtr;
                                VAR (*INOUT*) filename: ARRAY OF CHAR);

    (* The specified file is copied to outcid, which the caller has     *)
    (* already opened, with modifications as specified by SSI directives.*)

    CONST
        CtrlZ = CHR(26);

    VAR
        incid: ChanId;
        pos: CARDINAL;
        found, EOF: BOOLEAN;
        line, partline: ARRAY [0..1023] OF CHAR;

    BEGIN
        incid := OpenOldFile (filename, FALSE, FALSE);
        REPEAT
            ReadLine (incid, line);
            EOF := line[0] = CtrlZ;
            IF NOT EOF THEN
                WHILE line[0] <> Nul DO
                    Strings.FindNext ("<!--#", line, 0, found, pos);
                    IF found THEN
                        IF pos > 0 THEN

                            (* Copy over the leading part of the line. *)

                            Strings.Assign (line, partline);
                            partline[pos] := Nul;
                            FWriteString (outcid, partline);
                            Strings.Delete (line, 0, pos);
                        END (*IF*);
                        Strings.Delete (line, 0, 5);
                        HandleDirective (sess, incid, outcid, DirInfo, line);
                    END (*IF*);
                    IF line[0] <> Nul THEN
                        FWriteString (outcid, line);
                        line[0] := Nul;
                    END (*IF*);
                END (*WHILE*);
                FWriteLn (outcid);
            END (*IF*);
        UNTIL EOF;
        CloseFile (incid);
    END DoSSIProcess;

(************************************************************************)

PROCEDURE ProcessSSI (sess: Session;  D: Domain;
                       VAR (*INOUT*) filename, CurrentDir: ARRAY OF CHAR);

    (* The specified file is replaced by the modified version that      *)
    (* results from processing the SSI directives.  Assumption:         *)
    (* CurrentDir starts with a '/', and is a specification relative to *)
    (* the HTML root directory for this domain.                         *)

    VAR outcid: ChanId;
        DirInfo: DirInfoPtr;
        outfile: FilenameString;

    BEGIN
        NEW (DirInfo);
        DirInfo^.domain := D;
        Strings.Assign (CurrentDir, DirInfo^.CurrentDir);
        outcid := OpenTemporaryFile (outfile);
        DoSSIProcess (sess, outcid, DirInfo, filename);
        CloseFile (outcid);

        (* Now alter the filename parameter to give the caller the  *)
        (* altered file.  We mustn't delete the original file, but  *)
        (* the caller must delete the altered file after using it.  *)

        Strings.Assign (outfile, filename);
        DISPOSE (DirInfo);

    END ProcessSSI;

(************************************************************************)

END SSI.

