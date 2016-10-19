(**************************************************************************)
(*                                                                        *)
(*  Web server                                                            *)
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

IMPLEMENTATION MODULE ExecCGI;

        (********************************************************)
        (*                                                      *)
        (*                 Running a CGI script                 *)
        (*                                                      *)
        (*  This is logically part of module Requests, but I'm  *)
        (*  keeping it separate to simplify development and     *)
        (*  testing.  I might want to move this code later.     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            21 April 2015                   *)
        (*  Last edited:        22 April 2015                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, OS2;

FROM Names IMPORT
    (* type *)  FilenameIndex, FilenameString, ArgPointer;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Copy;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    (* An environment specification uses a "string of strings" format,  *)
    (* where each string is of the form "parameter=value" with a Nul    *)
    (* terminator.  The final Nul-terminated string must be followed    *)
    (* by another Nul.  This gives us a long string that may contain    *)
    (* internal Nul characters, so we cannot rely on the usual string   *)
    (* funtions to work.  In particular, we must keep track of the      *)
    (* total length, because scanning for a Nul won't work.  (Scanning  *)
    (* for a double Nul would work but would be inefficient.)  The      *)
    (* convention I use here is that the length value includes all the  *)
    (* internal Nuls but not the final one.                             *)

    EnvRecordPtr = POINTER TO
                        RECORD
                            content: ArgPointer;
                            length: CARDINAL;
                        END (*RECORD*);

(************************************************************************)
(*                   UPDATING AN ENVIRONMENT STRING                     *)
(************************************************************************)

PROCEDURE AddToEnvironmentString (VAR (*INOUT*) p: EnvRecordPtr;
                                   parameter, value: ARRAY OF CHAR);

    (* Adds a new parameter=value entry to the environment string. *)

    VAR oldlength, newlength, j, k: CARDINAL;
        q: ArgPointer;
        ch: CHAR;

    BEGIN
        IF p = NIL THEN
            NEW (p);
            p^.content := NIL;
            p^.length := 0;
        END (*IF*);
        IF p^.content = NIL THEN
            oldlength := 0;
        ELSE
            oldlength := p^.length;
        END (*IF*);
        newlength := oldlength + LENGTH(parameter) + LENGTH(value) + 2;
        ALLOCATE (q, newlength + 1);
        IF oldlength > 0 THEN
            Copy (p^.content, q, oldlength);
        END (*IF*);

        (* Note the extra one character to hold the terminating Nul. *)

        k := oldlength;  j := 0;
        LOOP
            ch := parameter[j];  INC(j);
            IF ch = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            q^[k] := ch;  INC(k);
        END (*LOOP*);
        q^[k] := '=';  INC(k);
        j := 0;
        LOOP
            ch := value[j];  INC(j);
            IF ch = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            q^[k] := ch;  INC(k);
        END (*LOOP*);
        q^[k] := Nul;  INC(k);

        (* Now the final terminator, after which we can *)
        (* deallocate the original string.              *)

        q^[k] := Nul;
        IF p^.content <> NIL THEN
            DEALLOCATE (p^.content, oldlength + 1);
        END (*IF*);
        p^.content := q;
        p^.length := newlength;

    END AddToEnvironmentString;

(************************************************************************)
(*                   EXECUTING A CGI SCRIPT OR PROGRAM                  *)
(************************************************************************)

PROCEDURE ExecProgram (progname: ARRAY OF CHAR;
                            VAR (*IN*) langstr: ARRAY OF CHAR;
                            VAR (*IN*) tempfile: ARRAY OF CHAR);

    (* Executes a CGI application.  Progname specifies the program or   *)
    (* script to be executed, optionally followed by a '?' followed by  *)
    (* parameters to be sent to the program.  Program output is put in  *)
    (* tempfile, which the caller must delete after use.  The langstr   *)
    (* variable says what languages the client prefers.                 *)

    CONST ONLength = 256;

    VAR j, result, pos: CARDINAL;
        penv: EnvRecordPtr;
        ArgString, progparams: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        found: BOOLEAN;

    BEGIN
        (* First separate out the parameters to be passed to the CGI    *)
        (* script or program.                                           *)

        Strings.FindNext ('?', progname, 0, found, pos);
        IF found THEN
            Strings.Extract (progname, pos+1, LENGTH(progname)-pos-1, progparams);
            progname[pos] := Nul;
        ELSE
            progparams[pos] := Nul;
        END (*IF*);

        (* The program parameters must be passed to the child process   *)
        (* as an environment variable.  The environment specification   *)
        (* is uses a "string of strings" format, where each string      *)
        (* is of the form "variable=value".                             *)

        penv := NIL;
        AddToEnvironmentString (penv, "QUERY_STRING", progparams);
        AddToEnvironmentString (penv, "HTTP_ACCEPT_LANGUAGE", langstr);

        ArgString := "CMD /C ";
        Strings.Append (progname, ArgString);
        Strings.Append (" >", ArgString);
        Strings.Append (tempfile, ArgString);

        (* Special rule for ArgString: it must be terminated by two Nul *)
        (* characters, and the program name and arguments must also be  *)
        (* separated by a Nul.  We have to insert the separating Nul    *)
        (* after everything else has been done, otherwise it would mess *)
        (* up the Strings.Append operation.                             *)

        (* For the purpose of these rules, CMD is the program name.     *)
        (* Everything else, including ProgName, is considered to be     *)
        (* part of the arguments.                                       *)

        j := LENGTH(ArgString) + 1;
        IF j <= MAX(FilenameIndex) THEN
            ArgString[j] := Nul;
        END (*IF*);
        ArgString[3] := Nul;
        result := OS2.DosExecPgm (FailureObjectName, ONLength,
                                  OS2.EXEC_SYNC, ArgString, penv^.content,
                                  ExitStatus, "CMD.EXE");

        (* Starting in background (code 457) is not an error. *)

        (* At present I'm not doing anything with the exit status. *)
        (*
        IF (result = 0) OR (result = 457) THEN
            result := ExitStatus.codeResult;
        ELSE
            result := 0;
        END (*IF*);
        *)

    END ExecProgram;

(************************************************************************)

END ExecCGI.

