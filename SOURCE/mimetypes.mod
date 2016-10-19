(**************************************************************************)
(*                                                                        *)
(*  Web server session manager                                            *)
(*  Copyright (C) 2016   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE MIMEtypes;

        (********************************************************)
        (*                                                      *)
        (*         Identifying the MIME type of a file          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 April 2015                    *)
        (*  Last edited:        14 August 2016                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, INIData;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  Exists, DeleteFile, OpenOldFile, OpenNewFile, CloseFile,
                ReadLine, FWriteChar, FWriteString, FWriteLn;

(************************************************************************)

CONST
    Nul = CHR(0);  CtrlZ = CHR(26);  Space = ' ';  Tab = CHR(9);
    MaxTypes = 1024;
    MaxExtensions = 1024;

TYPE
    ExtensionStr = ARRAY [0..7] OF CHAR;
    TypeStr = ARRAY [0..255] OF CHAR;

    Entry = RECORD
                ext:  ExtensionStr;
                typenum: CARDINAL;
            END (*RECORD*);

VAR
    TypeCount, ExtensionCount: CARDINAL;
    TypeTable: ARRAY [0..MaxTypes-1] OF TypeStr;
    ExtensionTable: ARRAY [0..MaxExtensions] OF Entry;

(************************************************************************)
(*                  FOR TESTING: DUMP THE STORED DATA                   *)
(************************************************************************)

(*
PROCEDURE DumpTable;

    (* Needed only while testing this module.  Dumps the MIME table *)
    (* to a text file.                                              *)

    CONST filename = "sorted.txt";

    VAR j: CARDINAL;  cid: ChanId;

    BEGIN
        IF Exists (filename) THEN
            DeleteFile (filename);
        END (*IF*);
        cid := OpenNewFile (filename, FALSE);
        IF ExtensionCount > 0 THEN
            FOR j := 0 TO ExtensionCount-1 DO
                FWriteString (cid, ExtensionTable[j].ext);
                FWriteChar (cid, " ");
                FWriteString (cid, TypeTable[ExtensionTable[j].typenum]);
                FWriteLn (cid);
            END (*FOR*);
        CloseFile (cid);
        END (*IF*);
    END DumpTable;
*)

(************************************************************************)
(*                    SEARCHING THE EXTENSION TABLE                     *)
(************************************************************************)

PROCEDURE LocateExtension (VAR (*IN*) ext: ARRAY OF CHAR;
                            VAR (*OUT*) pos: CARDINAL): BOOLEAN;

    (* If ext is in ExtensionTable, returns TRUE and pos set to its     *)
    (* position.  If not, returns FALSE with pos set to where it        *)
    (* can be inserted.                                                 *)

    VAR low, high: CARDINAL;
        comp: Strings.CompareResults;
        found: BOOLEAN;

    BEGIN
        low := 0;  high := ExtensionCount;
        IF high = 0 THEN
            (* An empty table is a special case. *)
            pos := 0;
            RETURN FALSE;
        END (*IF*);

        (* Let p = desired final value of pos.  *)
        (* Loop invariant:  low <= p < high     *)

        found := FALSE;  comp := Strings.equal;
        WHILE (NOT found) AND (high > low) DO
            pos := (low + high) DIV 2;
            (* Note low <= pos < high *)
            comp := Strings.Compare (ext, ExtensionTable[pos].ext);
            IF comp = Strings.equal THEN
                found := TRUE;
            ELSIF comp = Strings.less THEN
                (* Search the lower half of the range. *)
                high := pos;
            ELSE
                (* Search the upper half of the range. *)
                low := pos+1;
            END (*IF*);
        END (*WHILE*);
        IF comp = Strings.greater THEN
            (* Correct for undershoot. *)
            INC (pos);
        END (*IF*);
        RETURN found;
    END LocateExtension;

(************************************************************************)
(*                  MAPPING A FILE NAME TO A MIME TYPE                  *)
(************************************************************************)

PROCEDURE IdentifyType (VAR (*IN*) filename: ARRAY OF CHAR;
                           VAR (*OUT*) type: ARRAY OF CHAR);

    (* Returns the MIME type of a file.  Returns an empty string if the *)
    (* type is unknown.                                                 *)

    VAR L, pos: CARDINAL;
        found: BOOLEAN;
        extension: ExtensionStr;

    BEGIN
        pos := 0;
        L := LENGTH(filename) - 1;
        found := L > 0;
        IF found THEN
            Strings.FindPrev ('.', filename, L, found, pos);
            found := found AND (L > pos);
            IF found THEN
                Strings.Extract (filename, pos+1, L-pos, extension);
                Strings.Capitalize (extension);
            END (*IF*);
        END (*IF*);

        found := found AND LocateExtension (extension, pos);

        IF found THEN
            L := ExtensionTable[pos].typenum;
            Strings.Assign (TypeTable[L], type);
        ELSE
            Strings.Assign ("", type);
        END (*IF*);

    END IdentifyType;

(************************************************************************)
(*                    CREATING THE TRANSLATION TABLES                   *)
(************************************************************************)

PROCEDURE NewType (VAR (*IN*) type: TypeStr): CARDINAL;

    (* Stores one type name.  *)

    BEGIN
        IF TypeCount >= MaxTypes THEN
            WriteString ("ERROR: MIME type table overflow");
            WriteLn;
        ELSE
            TypeTable[TypeCount] := type;
            INC (TypeCount);
        END (*IF*);
        RETURN TypeCount-1;
    END NewType;

(************************************************************************)

PROCEDURE NewExtension (typenum: CARDINAL;  VAR (*IN*) ext: ExtensionStr);

    (* Stores one extension table entry.  *)
    (* This version produces a sorted table. *)

    VAR k, pos: CARDINAL;

    BEGIN
        IF ExtensionCount >= MaxExtensions THEN
            WriteString ("ERROR: MIME extension table overflow");
            WriteLn;
        ELSE
            Strings.Capitalize (ext);
            IF LocateExtension (ext, pos) THEN
                (* Duplicate entry, don't store the new one. *)
            ELSE

                (* Make a space for the new entry. *)

                k := ExtensionCount;
                INC (ExtensionCount);
                WHILE k > pos DO
                    ExtensionTable[k] := ExtensionTable[k-1];
                    DEC (k);
                END (*WHILE*);

                (* Insert the new entry. *)

                ExtensionTable[k].ext := ext;
                ExtensionTable[k].typenum := typenum;

            END (*IF*);
        END (*IF*);
    END NewExtension;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE LoadMIMETableFromFile;

    (* Fills in the MIME translation table from the data in MIME.cfg.   *)

    VAR line: ARRAY [0..1023] OF CHAR;

    (********************************************************************)

    PROCEDURE SkipSpaces;

        (* Deletes any leading space or tab characters from line. *)

        VAR k: CARDINAL;

        BEGIN
            k := 0;
            WHILE (line[k] = Space) OR (line[k] = Tab) DO
                INC (k);
            END (*WHILE*);
            IF k > 0 THEN
                Strings.Delete (line, 0, k);
            END (*IF*);
        END SkipSpaces;

    (********************************************************************)

    PROCEDURE GetNextToken (VAR (*OUT*) token: ARRAY OF CHAR);

        (* Removes the next substring from line. *)

        VAR pos: CARDINAL;  found: BOOLEAN;

        BEGIN
            Strings.FindNext (' ', line, 0, found, pos);
            IF NOT found THEN
                pos := Strings.Length(line);
            END (*IF*);
            Strings.Assign (line, token);
            token[pos] := Nul;
            Strings.Delete (line, 0, pos);
            SkipSpaces;
        END GetNextToken;

    (********************************************************************)

    VAR cid: ChanId;  k: CARDINAL;
        type: TypeStr;
        extension: ExtensionStr;

    BEGIN
        ExtensionCount := 0;  TypeCount := 0;
        cid := OpenOldFile ("MIME.cfg", FALSE, FALSE);
        IF cid = NoSuchChannel THEN
            WriteString ("Missing file MIME.cfg, no MIME types defined.");
            WriteLn;
        ELSE
            line[0] := Nul;
            WHILE line[0] <> CtrlZ DO
                ReadLine (cid, line);
                SkipSpaces;

                (* Ignore empty lines and comments. *)

                IF (line[0] <> Nul) AND (line[0] <> CtrlZ) AND (line[0] <> ';') THEN

                    GetNextToken (type);
                    k := NewType (type);

                    (* We have the type, now extract the extensions,    *)
                    (* and create a table entry for each extension.     *)

                    REPEAT
                        GetNextToken (extension);
                        IF extension[0] <> Nul THEN
                            NewExtension (k, extension);
                        END (*IF*);
                    UNTIL extension[0] = Nul;

                END (*IF*);

            END (*WHILE*);
            CloseFile (cid);
        END (*IF*);
    END LoadMIMETableFromFile;

(************************************************************************)

BEGIN
    LoadMIMETableFromFile;
    (*DumpTable;*)
END MIMEtypes.

