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

IMPLEMENTATION MODULE MIMEtypes;

        (********************************************************)
        (*                                                      *)
        (*         Identifying the MIME type of a file          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 April 2015                    *)
        (*  Last edited:        22 April 2015                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, INIData;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM Inet2Misc IMPORT
    (* proc *)  StringMatch;

(************************************************************************)

TYPE
    ExtensionStr = ARRAY [0..7] OF CHAR;
    TypeStr = ARRAY [0..63] OF CHAR;

    Entry = RECORD
                ext:  ExtensionStr;
                type: TypeStr;
            END (*RECORD*);

CONST
    Nul = CHR(0);
    MaxEntries = 512;

VAR
    Map: ARRAY [0..MaxEntries-1] OF Entry;
    EntryCount: CARDINAL;

(************************************************************************)
(*                  MAPPING A FILE NAME TO A MIME TYPE                  *)
(************************************************************************)

PROCEDURE IdentifyType (VAR (*IN*) filename: ARRAY OF CHAR;
                           VAR (*OUT*) type: ARRAY OF CHAR);

    (* Returns the MIME type of a file.  Returns an empty string    *)
    (* if the type is unknown.                                      *)

    (* At present just a dummy. *)

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
            END (*IF*);
        END (*IF*);

        IF found THEN
            (* Search our table of types. *)
            pos := 0;  found := FALSE;
            WHILE NOT found AND (pos < MaxEntries) DO
                found := StringMatch (Map[pos].ext, extension);
                INC (pos);
            END (*LOOP*);
        END (*IF*);

        IF found THEN
            Strings.Assign (Map[pos-1].type, type);
        ELSE
            Strings.Assign ("", type);
        END (*IF*);

    END IdentifyType;

(************************************************************************)
(*                    CREATING THE TRANSLATION TABLE                    *)
(************************************************************************)

PROCEDURE NewTableEntry (VAR (*IN*) ext: ARRAY OF CHAR;
                          VAR (*IN*) type: ARRAY OF CHAR);

    (* The 'line' string starts with a MIME type and continues with a   *)
    (* space-separated list of extensions.                              *)

    BEGIN
        IF EntryCount >= MaxEntries THEN
            WriteString ("ERROR: MIME table overflow");
            WriteLn;
        ELSE
            Strings.Assign (ext, Map[EntryCount].ext);
            Strings.Assign (type, Map[EntryCount].type);
            INC (EntryCount);
        END (*IF*);
    END NewTableEntry;

(************************************************************************)

PROCEDURE AddEntry (line: ARRAY OF CHAR);

    (* The 'line' string starts with a MIME type and continues with a   *)
    (* space-separated list of extensions.                              *)

    VAR type: TypeStr;  ext: ExtensionStr;
        pos0, pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.FindNext (' ', line, 0, found, pos);
        IF found THEN
            Strings.Extract (line, 0, pos, type);
            WHILE line[pos] = ' ' DO
                REPEAT
                    INC (pos);
                UNTIL line[pos] <> ' ';
                pos0 := pos;
                Strings.FindNext (' ', line, pos0, found, pos);
                IF NOT found THEN
                    pos := Strings.Length(line);
                END (*IF*);
                IF pos > pos0 THEN
                    Strings.Extract (line, pos0, pos-pos0, ext);
                    NewTableEntry (ext, type);
                END (*IF*);
            END (*WHILE*);
        ELSE
            WriteString ("ERROR: Faulty MIME table specification");
            WriteLn;
        END (*IF*);
    END AddEntry;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE LoadMIMETable (INIname: ARRAY OF CHAR;  UseTNI: BOOLEAN);

    (* Fills in the MIME translation table. *)

    VAR hini: INIData.HINI;
        state: INIData.StringReadState;
        string: ARRAY [0..255] OF CHAR;
        app, key: ARRAY [0..4] OF CHAR;

    BEGIN
        EntryCount := 0;
        hini := INIData.OpenINIFile (INIname, UseTNI);
        app := "$SYS";  key := "MIME";
        INIData.GetStringList (hini, app, key, state);
        LOOP
            INIData.NextString (state, string);
            IF string[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            AddEntry (string);
        END (*LOOP*);
        INIData.CloseStringList (state);
        INIData.CloseINIFile (hini);
    END LoadMIMETable;

(************************************************************************)

END MIMEtypes.

