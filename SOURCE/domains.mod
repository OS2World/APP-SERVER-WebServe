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
        (*  Last edited:        29 April 2015                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, INIData;

FROM Types IMPORT
    (* type *)  CARD64;

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
    (* proc *)  FirstDirEntry, DirSearchDone;

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
(*                            LOCATING A FILE                           *)
(************************************************************************)

PROCEDURE LocateFile (D: Domain;  VAR (*IN*) URL: ARRAY OF CHAR;
                              VAR (*OUT*) filename: ARRAY OF CHAR;
                                VAR (*OUT*) lastmodified: ARRAY OF CHAR;
                                    VAR (*OUT*) size: CARD64;
                                      VAR (*OUT*) CGI: BOOLEAN): BOOLEAN;

    (* Translates a URL into a file name, also returns its size.  If    *)
    (* CGI is true then this is an executable.                          *)

    VAR dir, args: FilenameString;
        DirEnt: DirectoryEntry;
        pos: CARDINAL;
        found, Qfound: BOOLEAN;

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
                Strings.Append ("index.html", URL);
            END (*IF*);
        END (*IF*);

        Strings.Assign (dir, filename);
        Strings.Append (URL, filename);
        found := FirstDirEntry (filename, FALSE, FALSE, DirEnt);
        IF found THEN
            (* A file of this name exists. *)
            size := DirEnt.size;
            PackedDateTimeToGMT (DirEnt.datePkd, DirEnt.timePkd,
                                                    lastmodified);
        ELSE
            (* There might still be a directory of this name,   *)
            (* but in the present version I don't handle that.  *)
        END (*IF*);
        DirSearchDone (DirEnt);

        (* Restore CGI arguments if they have been removed. *)

        IF CGI AND Qfound THEN
            Strings.Append (args, filename);
        END (*IF*);

        RETURN found;

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

