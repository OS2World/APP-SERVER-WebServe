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

IMPLEMENTATION MODULE Domains;

        (********************************************************)
        (*                                                      *)
        (*            Operations within a web domain            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            3 April 2015                    *)
        (*  Last edited:        9 May 2015                      *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, INIData;

FROM Types IMPORT
    (* type *)  CARD64;

FROM LONGLONG IMPORT
    (* const*)  Zero64;

FROM Names IMPORT
    (* type *)  FilenameString, DomainName, HostName;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Inet2Misc IMPORT
    (* proc *)  StringMatch;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

FROM MyClock IMPORT
    (* proc *)  PackedDateTimeToGMT;

FROM FileOps IMPORT
    (* type *)  DirectoryEntry,
    (* proc *)  FirstDirEntry, DirSearchDone, Exists;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    HostList = POINTER TO
                 RECORD
                     next: HostList;
                     this: HostName;
                 END (*RECORD*);

    Domain = POINTER TO
                 RECORD
                     name: DomainName;
                     othernames: HostList;
                     RootDir: FilenameString;
                     CGIDir: FilenameString;
                 END (*RECORD*);

    ListOfDomains = POINTER TO
                        RECORD
                            next: ListOfDomains;
                            this: Domain;
                        END (*RECORD*);

VAR
    DomainList: RECORD
                    access: Lock;
                    head: ListOfDomains;
                END (*RECORD*);

(************************************************************************)
(*                  OPENING AND CLOSING A DOMAIN RECORD                 *)
(************************************************************************)

PROCEDURE HostMatch (D: Domain;  VAR (*IN*) name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name is an acceptable name for domain D. *)

    VAR L: HostList;

    BEGIN
        IF StringMatch (name, D^.name) THEN
            RETURN TRUE;
        END (*IF*);
        L := D^.othernames;
        LOOP
            IF L = NIL THEN
                RETURN FALSE;
            END (*IF*);
            IF WildMatch (name, L^.this) THEN
                RETURN TRUE;
            END (*IF*);
            L := L^.next;
        END (*LOOP*);
    END HostMatch;

(************************************************************************)

PROCEDURE OpenDomain (name: ARRAY OF CHAR): Domain;

    (* Opens a domain record. *)

    VAR L: ListOfDomains;

    BEGIN
        Obtain (DomainList.access);
        L := DomainList.head;
        WHILE (L <> NIL) AND NOT HostMatch(L^.this, name) DO
            L := L^.next;
        END (*WHILE*);
        Release (DomainList.access);
        IF L = NIL THEN
            RETURN NIL;
        ELSE
            RETURN L^.this;
        END (*IF*);
    END OpenDomain;

(************************************************************************)

PROCEDURE CloseDomain (VAR (*INOUT*) D: Domain);

    (* Closes the domain record.  We don't actually remove the record,  *)
    (* we just delete the pointer to it.                                *)

    BEGIN
        D := NIL;
    END CloseDomain;

(************************************************************************)
(*                          FILE PROPERTIES                             *)
(************************************************************************)

PROCEDURE SizeOf (VAR (*IN*) filename: ARRAY OF CHAR): CARD64;

    (* Returns the file size. *)

    VAR DirEnt: DirectoryEntry;
        found: BOOLEAN;

    BEGIN
        found := FirstDirEntry (filename, FALSE, FALSE, DirEnt);
        DirSearchDone (DirEnt);
        IF found THEN
            RETURN DirEnt.size;
        ELSE
            RETURN Zero64;
        END (*IF*);
    END SizeOf;

(************************************************************************)

PROCEDURE DirOf (D: Domain;  VAR (*IN*) filename: ARRAY OF CHAR;
                             VAR (*OUT*) directory: ARRAY OF CHAR);

    (* Sets directory equal to the directory part of filename.  The     *)
    (* specified filename is a full path name, and is known to be in    *)
    (* the directory tree for this domain. The result starts with a     *)
    (* '/', and is relative to the HTML root directory for the domain.  *)

    VAR pos1, pos2: CARDINAL;
        found1, found2: BOOLEAN;

    BEGIN
        Strings.Assign (filename, directory);

        (* Since we know that filename was the output of LocateFile,    *)
        (* below, we can simply strip the D^.rootdir from the front     *)
        (* without needing to do any string matching.                   *)

        Strings.Delete (directory, 0, LENGTH(D^.RootDir));

        (* Now chop off the tail of the result. *)

        Strings.FindPrev ('/', directory, LENGTH(directory), found1, pos1);
        Strings.FindPrev ('\', directory, LENGTH(directory), found2, pos2);
        IF found2 THEN
            IF (NOT found1) OR (pos2 > pos1) THEN
                pos1 := pos2;
            END (*IF*);
            found1 := TRUE;
        END (*IF*);

        IF found1 THEN
            directory[pos1] := Nul;
        ELSE
            Strings.Assign ("/", directory);
        END (*IF*);

    END DirOf;

(************************************************************************)
(*                            LOCATING A FILE                           *)
(************************************************************************)

PROCEDURE AppendDefaultFilename (D: Domain;  VAR (*INOUT*) name: ARRAY OF CHAR);

    (* To be called when name ends with a '/'.  We append one of        *)
    (* "index.shtml", "index.html", "index.htm", whichever exists.      *)

    VAR base: FilenameString;

    (********************************************************************)

    PROCEDURE Try (f: ARRAY OF CHAR): BOOLEAN;

        VAR testname: FilenameString;

        BEGIN
            Strings.Assign (base, testname);
            Strings.Append (f, testname);
            IF NOT Exists (testname) THEN
                RETURN FALSE;
            END (*IF*);
            Strings.Append (f, name);
            RETURN TRUE;
        END Try;

    (********************************************************************)

    BEGIN
        Strings.Assign (D^.RootDir, base);
        Strings.Append (name, base);

        IF NOT Try("index.shtml") THEN
            IF NOT Try("index.html") THEN
                IF NOT Try("index.htm") THEN
                    (* Leave name unchanged. *)
                END (*IF*);
            END (*IF*);
        END (*IF*);

    END AppendDefaultFilename;

(************************************************************************)

PROCEDURE LocateFile (D: Domain;  VAR (*IN*) URL: ARRAY OF CHAR;
                        VAR (*OUT*) filename: ARRAY OF CHAR;
                          VAR (*OUT*) lastmodified: ARRAY OF CHAR;
                              VAR (*OUT*) size: CARD64;
                                VAR (*OUT*) CGI, SHTML: BOOLEAN): BOOLEAN;

    (* Translates a URL into a file name, also returns its size.  If    *)
    (* CGI is true then this is an executable.                          *)

    VAR dir, args: FilenameString;
        DirEnt: DirectoryEntry;
        pos: CARDINAL;
        found, Qfound, filefound: BOOLEAN;

    BEGIN
        size := CARD64{0,0};
        pos := 0;  Qfound := FALSE;

        (* See whether the URL starts with /cgi-bin. *)

        Strings.Assign (URL, dir);
        dir[8] := Nul;
        CGI := StringMatch (dir, "/cgi-bin");
        IF CGI THEN
            dir := D^.CGIDir;
            Strings.Delete (URL, 0, 8);

            (* Temporarily remove the '?' and what follows. *)

            Strings.FindNext ('?', URL, 0, Qfound, pos);
            IF Qfound THEN
                Strings.Extract (URL, pos, LENGTH(URL)-pos, args);
                URL[pos] := Nul;
            END (*IF*);
        ELSE
            dir := D^.RootDir;
            IF URL[LENGTH(URL)-1] = '/' THEN
                AppendDefaultFilename (D, URL);
            END (*IF*);
        END (*IF*);

        (* See whether the name ends with .shtml  *)

        IF LENGTH(URL) < 6 THEN
            SHTML := FALSE;
        ELSE
            Strings.Assign (URL, filename);
            Strings.Delete (filename, 0, LENGTH(filename)-6);
            SHTML := StringMatch (filename, ".shtml");
        END (*IF*);

        (* Convert the filename to a fully specified path string. *)

        Strings.Assign (dir, filename);
        Strings.Append (URL, filename);
        filefound := FirstDirEntry (filename, FALSE, FALSE, DirEnt);
        IF filefound THEN
            (* A file of this name exists. *)
            size := DirEnt.size;
            PackedDateTimeToGMT (DirEnt.datePkd, DirEnt.timePkd,
                                                    lastmodified);

            (* Special case where filename contains a wildcard. *)

            Strings.FindNext ("*", filename, 0, found, pos);
            IF found THEN
                Strings.FindPrev ("/", filename, pos, found, pos);
                filename[pos+1] := Nul;
                Strings.Append (DirEnt.name, filename);
            END (*IF*);

        ELSE
            (* There might still be a directory of this name,   *)
            (* but in the present version I don't handle that.  *)
        END (*IF*);
        DirSearchDone (DirEnt);

        (* Restore CGI arguments if they have been removed. *)

        IF CGI AND Qfound THEN
            Strings.Append (args, filename);
        END (*IF*);

        RETURN filefound;

    END LocateFile;

(************************************************************************)
(*                 CREATING THE LIST OF DOMAINS WE SUPPORT              *)
(************************************************************************)

PROCEDURE AddDomain (D: Domain);

    (* Adds this domain to the master list.  We assume that the caller  *)
    (* has exclusive access to the master list.                         *)

    VAR L: ListOfDomains;

    BEGIN
        IF D <> NIL THEN
            NEW (L);
            L^.this := D;
            L^.next := DomainList.head;
            DomainList.head := L;
        END (*IF*);
    END AddDomain;

(************************************************************************)

PROCEDURE StripTrailingSlash (VAR (*INOUT*) str: ARRAY OF CHAR);

    (* Removes any final '/' or '\' from str. *)

    VAR L: CARDINAL;

    BEGIN
        L := LENGTH(str);
        IF L > 0 THEN
            DEC (L);
            IF (str[L] = '/') OR (str[L] = '\') THEN
                str[L] := Nul;
            END (*IF*);
        END (*IF*);
    END StripTrailingSlash;

(************************************************************************)

PROCEDURE CreateDomainList (INIname: ARRAY OF CHAR;  UseTNI: BOOLEAN);

    (* Reads the domain data from the INI file. *)

    VAR D: Domain;  prev, H: HostList;
        hini: INIData.HINI;
        state, state2: INIData.StringReadState;
        name, CGIDir: FilenameString;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..8] OF CHAR;

    BEGIN
        Obtain (DomainList.access);
        DomainList.head := NIL;
        hini := INIData.OpenINIFile (INIname, UseTNI);
        app := "$SYS";  key := "Domains";
        INIData.GetStringList (hini, app, key, state);
        LOOP
            INIData.NextString (state, name);
            IF name[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            NEW (D);
            Strings.Assign (name, D^.name);
            IF NOT INIData.INIGetString (hini, name, "HTMLRoot", D^.RootDir) THEN
                D^.RootDir := "HTMLRoot";
            END (*IF*);
            StripTrailingSlash (D^.RootDir);
            IF NOT INIData.INIGetString (hini, name, "CGIDir", CGIDir) THEN
                CGIDir := "cgi-bin";
            END (*IF*);
            StripTrailingSlash (CGIDir);

            (* If the CGI directory is specified as a non-absolute path *)
            (* we interpret is as relative to the HTML root.            *)

            IF (CGIDir[0] = '/') OR (CGIDir[0] = '\') THEN
                D^.CGIDir := CGIDir;
            ELSE
                D^.CGIDir := D^.RootDir;
                Strings.Append ('/', D^.CGIDir);
                Strings.Append (CGIDir, D^.CGIDir);
            END (*IF*);

            (* Alternative names for the domain.  Wildcards are allowed. *)

            D^.othernames := NIL;
            app := "$SYS";  key := "names";
            prev := NIL;
            INIData.GetStringList (hini, D^.name, key, state2);
            LOOP
                INIData.NextString (state2, name);
                IF name[0] = Nul THEN
                    EXIT (*LOOP*);
                END (*IF*);
                NEW (H);
                IF prev = NIL THEN
                    D^.othernames := H;
                ELSE
                    prev^.next := H;
                END (*IF*);
                H^.next := NIL;
                Strings.Assign (name, H^.this);
                prev := H;
            END (*LOOP*);
            INIData.CloseStringList (state2);

            AddDomain (D);

        END (*LOOP*);
        INIData.CloseStringList (state);
        INIData.CloseINIFile (hini);
        Release (DomainList.access);
    END CreateDomainList;

(************************************************************************)

PROCEDURE DiscardHostList (VAR (*INOUT*) H: HostList);

    (* Discards a list of hosts. *)

    VAR next: HostList;

    BEGIN
        WHILE H <> NIL DO
            next := H^.next;
            DISPOSE (H);
            H := next;
        END (*WHILE*);
    END DiscardHostList;

(************************************************************************)

PROCEDURE DiscardDomain (VAR (*INOUT*) D: Domain);

    (* Discards a domain structure. *)

    BEGIN
        DiscardHostList (D^.othernames);
        DISPOSE (D);
    END DiscardDomain;

(************************************************************************)

PROCEDURE RemoveDomainList;

    (* Removes the entire master list structure. *)

    VAR L, next: ListOfDomains;

    BEGIN
        Obtain (DomainList.access);
        L := DomainList.head;
        WHILE L <> NIL DO
            next := L^.next;
            DiscardDomain (L^.this);
            DISPOSE (L);
            L := next;
        END (*WHILE*);
        DomainList.head := NIL;
        Release (DomainList.access);
    END RemoveDomainList;

(************************************************************************)
(*                            INITIALISATION                            *)
(************************************************************************)

PROCEDURE UpdateDomainList (INIname: ARRAY OF CHAR;  UseTNI: BOOLEAN);

    (* Loads our domain information from the INI file. *)

    BEGIN
        RemoveDomainList;
        CreateDomainList (INIname, UseTNI);
    END UpdateDomainList;

(************************************************************************)

BEGIN
    CreateLock (DomainList.access);
    DomainList.head := NIL;
FINALLY
    RemoveDomainList;
    DestroyLock (DomainList.access);
END Domains.

