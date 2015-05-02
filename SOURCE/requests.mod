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

IMPLEMENTATION MODULE Requests;

        (********************************************************)
        (*                                                      *)
        (*            Handle requests to web server             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2015                    *)
        (*  Last edited:        27 April 2015                   *)
        (*  Status:             OK                              *)
        (*          Will probably need to add more headers      *)
        (*          in response after checking RFC 2616.        *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(* STILL TO BE IMPLEMENTED:                                             *)
(*      Continuation lines in headers, see LWS definition               *)
(*                                                                      *)
(* TO CHECK:                                                            *)
(*      (Nothing at present)                                            *)
(*                                                                      *)
(* FOR FURTHER CONSIDERATION:                                           *)
(*      Handle one or all of the If-Modified-Since, If-Unmodified-Since,*)
(*                or If-Range header fields.                            *)
(*      Handle "partial GET" requested by a Range header.               *)
(*                                                                      *)
(************************************************************************)

IMPORT Strings;

FROM SYSTEM IMPORT
    (* type *)  LOC,
    (* proc *)  CAST;

FROM Names IMPORT
    (* type *)  FilenameString, HostName;

FROM Types IMPORT
    (* type *)  CARD64;

FROM LONGLONG IMPORT
    (* const*)  Zero64,
    (* proc *)  Compare64, Sub64;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM WSV IMPORT
    (* const*)  version;

FROM Sockets IMPORT
    (* type *)  Socket, SockAddr,
    (* proc *)  getpeername, send;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, INIValid, CloseINIFile, INIGet, INIGetString;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached;

FROM InetUtilities IMPORT
    (* proc *)  ConvertCard64;

FROM TransLog IMPORT
    (* proc *)  OpenLogContext, CloseLogContext,
                StartTransactionLogging, SetProcname, SetSyslogHost;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Inet2Misc IMPORT
    (* proc *)  WaitForSocketOut, IPToString;

FROM MyClock IMPORT
    (* proc *)  FormatCurrentDateTime, CurrentDateAndTimeGMT, CompareDateStrings;

FROM TransLog IMPORT
    (* type *)  LogContext, TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM InStream IMPORT
    (* type *)  IStream,
    (* proc *)  OpenIStream, CloseIStream, GetLine;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, DirectoryEntry,
    (* proc *)  OpenOldFile, OpenAtEnd, CloseFile, Exists, ReadRaw, ReadLine,
                FWriteChar, FWriteString, FWriteLJCard64, FWriteLn,
                DeleteFile, FirstDirEntry, DirSearchDone;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  UpdateDomainList, OpenDomain, CloseDomain, LocateFile;

FROM MIMEtypes IMPORT
    (* proc *)  LoadMIMETable, IdentifyType;

FROM ExecCGI IMPORT
    (* proc *)  ExecProgram;

(************************************************************************)

CONST
    Nul = CHR(0);  Space = ' ';  CR = CHR(13);  LF = CHR(10);
    NilDomain = CAST(Domain, NIL);

VAR CRLF: ARRAY [0..1] OF CHAR;

TYPE
    Session = POINTER TO
                  RECORD
                      LogID: TransactionLogID;
                      socket: Socket;
                      instream: IStream;
                      domain: Domain;
                      acceptlanguage: ARRAY [0..255] OF CHAR;
                      mustclose: BOOLEAN;
                      Host: ARRAY [0..255] OF CHAR;

                      (* The remaining fields are specific  *)
                      (* to the current transaction.        *)

                      ReqBuffer: ARRAY [0..1023] OF CHAR;
                      URL:  ARRAY [0..1023] OF CHAR;
                      lastmodified: ARRAY [0..31] OF CHAR;
                      ifmodifiedsince: ARRAY [0..31] OF CHAR;

                  END (*RECORD*);

VAR
    (* Log context for the program. *)

    LogCtx: LogContext;

    (* String used in creating a unique file name. *)

    NextName: ARRAY [0..7] OF CHAR;
    NextNameLock: Lock;

    (* Common log. *)

    CommonLogLock: Lock;
    CommonLogName: FilenameString;
    CommonLogEnabled: BOOLEAN;

    (* Specifies how much to put in the transaction log. *)

    DetailedLogging: BOOLEAN;

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
(*                              COMMON LOG                              *)
(************************************************************************)

PROCEDURE MakeCommonLogEntry (sess: Session;
                                status: ARRAY OF CHAR;  count: CARD64);

    (* Adds an entry to the common.log file.  Format is                 *)
    (* 127.0.0.1 - - [10/Oct/2000:13:55:36 -0700]                       *)
    (*                       "GET /apache_pb.gif HTTP/1.0" 200 2326     *)
    (* (all in one line), where the first address is the client IP      *)
    (* address, and the last two numbers are the status code in our     *)
    (* response, and the number of octets transferred.                  *)

    VAR peer: SockAddr;  size: CARDINAL;
        cid: ChanId;
        Buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        size := SIZE(peer);
        IF getpeername (sess^.socket, peer, size) THEN
            peer.in_addr.addr := 0;
        END (*IF*);
        Obtain (CommonLogLock);
        cid := OpenAtEnd (CommonLogName);
        IPToString (peer.in_addr.addr, FALSE, Buffer);
        FWriteString (cid, Buffer);
        FWriteString (cid, " - - ");
        FormatCurrentDateTime ('[dd/mmm/yyyy:HH:MM:SS zzzz] ', FALSE, Buffer);
        FWriteString (cid, Buffer);
        FWriteChar (cid, '"');
        FWriteString (cid, sess^.ReqBuffer);
        FWriteString (cid, '" ');
        FWriteString (cid, status);
        FWriteChar (cid, ' ');
        FWriteLJCard64 (cid, count);
        FWriteLn (cid);
        CloseFile (cid);
        Release (CommonLogLock);
    END MakeCommonLogEntry;

(************************************************************************)
(*                            MISCELLANEOUS                             *)
(************************************************************************)

PROCEDURE BodySize (file: ARRAY OF CHAR): CARD64;

    (* Returns the size of the file, minus the size of the header lines.*)


    VAR DirEnt: DirectoryEntry;
        cid: ChanId;
        L: CARDINAL;
        size: CARD64;
        found, inheader: BOOLEAN;
        line: ARRAY [0..1023] OF CHAR;

    BEGIN
        (* Get the total file size. *)

        found := FirstDirEntry (file, FALSE, FALSE, DirEnt);
        IF found THEN
            size := DirEnt.size;
        ELSE
            size := Zero64;
        END (*IF*);
        DirSearchDone (DirEnt);
        IF Compare64 (size, Zero64) > 0 THEN
            cid := OpenOldFile (file, FALSE, FALSE);
            inheader := TRUE;
            WHILE inheader DO
                ReadLine (cid, line);
                L := LENGTH(line);
                inheader := (L > 0);
                INC (L, 2);
                IF (size.high > 0) OR (L >= size.low) THEN
                    size := Zero64;
                ELSE
                    Sub64 (size, L);
                END (*IF*);
            END (*WHILE*);
            CloseFile (cid);
        END (*IF*);
        RETURN size;
    END BodySize;

(************************************************************************)
(*                           OUTPUT TO SOCKET                           *)
(************************************************************************)

PROCEDURE SendEOL (S: Socket);

    (* Sends a CRLF. *)

    BEGIN
        send (S, CRLF, 2, 0);
    END SendEOL;

(************************************************************************)

PROCEDURE PutAndLogLine (S: Socket;  LogID: TransactionLogID;
                                text: ARRAY OF CHAR;  always: BOOLEAN);

    (* Sends one line, plus CRLF terminator, with a copy to the     *)
    (* transaction log.  If always is FALSE then we only log this   *)
    (* if DetailedLogging is TRUE.                                  *)

    VAR logtext: ARRAY [0..1023] OF CHAR;

    BEGIN
        IF always OR DetailedLogging THEN
            logtext := "> ";
            Strings.Append (text, logtext);
            LogTransaction (LogID, logtext);
        END (*IF*);
        IF text[0] <> Nul THEN
            send (S, text, LENGTH(text), 0);
        END (*IF*);
        SendEOL (S);
    END PutAndLogLine;

(************************************************************************)

PROCEDURE PutFile (S: Socket; id: ChanId);

    (* Sends from an already-opened file. *)

    CONST BufferSize = 65536;

    VAR BuffPtr: POINTER TO ARRAY [0..BufferSize-1] OF LOC;
        xferred: CARDINAL;  success: BOOLEAN;

    BEGIN
        NEW (BuffPtr);
        success := TRUE;
        LOOP
            ReadRaw (id, BuffPtr^, BufferSize, xferred);
            IF xferred = 0 THEN
                EXIT(*LOOP*)
            END (*IF*);
            IF WaitForSocketOut (S, MAX(CARDINAL)) > 0 THEN
                xferred := send (S, BuffPtr^, xferred, 0);
            ELSE
                xferred := MAX(CARDINAL);
            END (*IF*);
            IF xferred = MAX(CARDINAL) THEN
                success := FALSE;
            END (*IF*);
            IF NOT success THEN
                EXIT(*LOOP*)
            END (*IF*);
        END (*LOOP*);
        DISPOSE (BuffPtr);
    END PutFile;

(************************************************************************)

PROCEDURE SendFile (S: Socket;  VAR (*IN*) name: ARRAY OF CHAR);

    (* Sends file 'name' to socket S. *)

    VAR cid: ChanId;

    BEGIN
        cid := OpenOldFile (name, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            PutFile (S, cid);
            CloseFile (cid);
        END (*IF*);
    END SendFile;

(************************************************************************)
(*                   OPENING AND CLOSING A SESSION                      *)
(************************************************************************)

PROCEDURE OpenSession (S: Socket;  ID: TransactionLogID): Session;

    (* Creates the session state for a new session. *)

    VAR result: Session;

    BEGIN
        NEW (result);
        result^.LogID := ID;
        result^.socket := S;
        result^.instream := OpenIStream (S);
        result^.domain := NilDomain;
        result^.mustclose := FALSE;
        result^.Host := "";
        result^.URL := "";
        result^.ReqBuffer := "";
        result^.lastmodified := "";
        result^.ifmodifiedsince := "";
        result^.acceptlanguage := "en";
        RETURN result;
    END OpenSession;

(************************************************************************)

PROCEDURE CloseSession (VAR (*INOUT*) sess: Session);

    (* Terminates a session. *)

    BEGIN
        IF sess <> NIL THEN
            CloseIStream (sess^.instream);
            CloseDomain (sess^.domain);
            DISPOSE (sess);
        END (*IF*);
    END CloseSession;

(************************************************************************)
(*                     RESPONSE FOR FAILURE CASES                       *)
(************************************************************************)

PROCEDURE FaultyRequestResponse (sess: Session);

    VAR S: Socket;

    BEGIN
        S := sess^.socket;
        PutAndLogLine (S, sess^.LogID, "HTTP/1.1 400 Bad Request", TRUE);
        PutAndLogLine (S, sess^.LogID, "", FALSE);
        IF CommonLogEnabled THEN
            MakeCommonLogEntry (sess, "400", Zero64);
        END (*IF*);
    END FaultyRequestResponse;

(************************************************************************)

PROCEDURE NotFoundResponse (sess: Session);

    VAR S: Socket;  ID: TransactionLogID;
        message: ARRAY [0..63] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;

    BEGIN
        S := sess^.socket;
        ID := sess^.LogID;
        PutAndLogLine (S, ID, "HTTP/1.1 404 Not Found", TRUE);
        message := "Date: ";
        CurrentDateAndTimeGMT (parambuffer);
        Strings.Append (parambuffer, message);
        PutAndLogLine (S, ID, message, FALSE);
        PutAndLogLine (S, ID, "Content-Type: text/plain", FALSE);
        PutAndLogLine (S, ID, "Content-Length: 15", FALSE);
        PutAndLogLine (S, ID, "", FALSE);
        PutAndLogLine (S, ID, "404 Not Found", FALSE);
        IF CommonLogEnabled THEN
            MakeCommonLogEntry (sess, "404", CARD64{15,0});
        END (*IF*);
    END NotFoundResponse;

(************************************************************************)

PROCEDURE NotImplementedResponse (sess: Session);

    VAR S: Socket;

    BEGIN
        S := sess^.socket;
        PutAndLogLine (S, sess^.LogID, "HTTP/1.1 501 Not Implemented", TRUE);
        PutAndLogLine (S, sess^.LogID, "", FALSE);
        IF CommonLogEnabled THEN
            MakeCommonLogEntry (sess, "501", Zero64);
        END (*IF*);
    END NotImplementedResponse;

(************************************************************************)
(*                 HANDLING THE GET AND HEAD COMMANDS                   *)
(************************************************************************)

PROCEDURE HandleGETcommand (sess: Session;  IncludeBody: BOOLEAN);

    VAR message: ARRAY [0..1023] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;
        filename, tempfile: FilenameString;
        S: Socket;  CGI, dontsend: BOOLEAN;
        pos: CARDINAL;
        ID: TransactionLogID;
        size: CARD64;

    BEGIN
        ID := sess^.LogID;
        IF sess^.domain = NilDomain THEN
            sess^.domain := OpenDomain (sess^.Host);
        END (*IF*);
        S := sess^.socket;

        IF sess^.domain = NilDomain THEN

            NotFoundResponse (sess);

        ELSIF LocateFile (sess^.domain, sess^.URL, filename,
                                  sess^.lastmodified, size, CGI) THEN

            dontsend := FALSE;
            IF NOT CGI AND (sess^.ifmodifiedsince[0] <> Nul) THEN
                dontsend := CompareDateStrings (sess^.lastmodified,
                                        sess^.ifmodifiedsince) <= 0;
            END (*IF*);

            IF dontsend THEN
                PutAndLogLine (S, ID, "HTTP/1.1 304 Not Modified", TRUE);
                PutAndLogLine (S, ID, "", FALSE);
                IF CommonLogEnabled THEN
                    MakeCommonLogEntry (sess, "304", Zero64);
                END (*IF*);
            ELSE
                PutAndLogLine (S, ID, "HTTP/1.1 200 Document follows", TRUE);

                message := "Server: WebServe/";
                Strings.Append (version, message);
                PutAndLogLine (S, ID, message, FALSE);

                message := "Date: ";
                CurrentDateAndTimeGMT (parambuffer);
                Strings.Append (parambuffer, message);
                PutAndLogLine (S, ID, message, FALSE);

                PutAndLogLine (S, ID, "Accept-Ranges: none", FALSE);
                IF sess^.mustclose THEN
                    PutAndLogLine (S, ID, "Connection: close", FALSE);
                ELSE
                    PutAndLogLine (S, ID, "Connection: keep-alive", FALSE);
                END (*IF*);

                IF CGI THEN
                    (* Execute CGI application. *)

                    MakeNewFilename (tempfile);
                    ExecProgram (filename, sess^.acceptlanguage, tempfile);
                    size := BodySize (tempfile);
                    message := "Content-Length: ";
                    pos := Strings.Length (message);
                    ConvertCard64 (size, message, pos);
                    message[pos] := Nul;
                    PutAndLogLine (S, ID, message, FALSE);

                    IF IncludeBody THEN

                        (* We expect that tempfile also includes some   *)
                        (* header lines, plus the blank line that       *)
                        (* terminates the header.                       *)

                        SendFile (S, tempfile);
                    ELSE
                        PutAndLogLine (S, ID, "", FALSE);
                        size := Zero64;
                    END (*IF*);
                    DeleteFile (tempfile);

                ELSE
                    (* Return the file. *)

                    IF sess^.lastmodified[0] <> Nul THEN
                        message := "Last-Modified: ";
                        Strings.Append (sess^.lastmodified, message);
                        PutAndLogLine (S, ID, message, FALSE);
                    END (*IF*);

                    message := "Content-Length: ";
                    pos := Strings.Length (message);
                    ConvertCard64 (size, message, pos);
                    message[pos] := Nul;
                    PutAndLogLine (S, ID, message, FALSE);

                    IdentifyType (filename, parambuffer);
                    IF parambuffer[0] <> Nul THEN
                        message := "Content-Type: ";
                        Strings.Append (parambuffer, message);
                        PutAndLogLine (S, ID, message, FALSE);
                    END (*IF*);

                    PutAndLogLine (S, ID, "", FALSE);

                    IF IncludeBody THEN
                        SendFile (S, filename);
                    ELSE
                        size := Zero64;
                    END (*IF*);

                END (*IF CGI*);

                IF CommonLogEnabled THEN
                    MakeCommonLogEntry (sess, "200", size);
                END (*IF*);

            END (*IF LocateFile*);

        ELSE
            (* File not found, cannot satisfy request. *)

            PutAndLogLine (S, ID, "HTTP/1.1 404 Not Found", TRUE);
            message := "Date: ";
            CurrentDateAndTimeGMT (parambuffer);
            Strings.Append (parambuffer, message);
            PutAndLogLine (S, ID, message, FALSE);
            PutAndLogLine (S, ID, "Content-Type: text/plain", FALSE);
            PutAndLogLine (S, ID, "Content-Length: 15", FALSE);
            PutAndLogLine (S, ID, "", FALSE);
            PutAndLogLine (S, ID, "404 Not Found", FALSE);
            IF CommonLogEnabled THEN
                MakeCommonLogEntry (sess, "404", CARD64{15,0});
            END (*IF*);

        END (*IF*);

    END HandleGETcommand;

(************************************************************************)
(*                  PARSING THE HEADER OF A REQUEST                     *)
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

PROCEDURE MatchLeading (VAR (*IN*) buffer: ARRAY OF CHAR;
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

PROCEDURE ParseParameters (sess: Session);

    VAR ReqBuffer, message: ARRAY [0..511] OF CHAR;
        InHeader: BOOLEAN;

    BEGIN
        InHeader := TRUE;
        WHILE InHeader DO
            IF GetLine (sess^.instream, ReqBuffer) THEN
                IF DetailedLogging THEN
                    message := "< ";
                    Strings.Append (ReqBuffer, message);
                    LogTransaction (sess^.LogID, message);
                END (*IF*);

                IF ReqBuffer[0] = Nul THEN
                    InHeader := FALSE;

                ELSIF MatchLeading (ReqBuffer, "Accept-Language:") THEN
                    SkipLeadingSpaces (ReqBuffer);
                    Strings.Assign (ReqBuffer, sess^.acceptlanguage);

                ELSIF MatchLeading (ReqBuffer, "Connection:") THEN
                    SkipLeadingSpaces (ReqBuffer);
                    sess^.mustclose := MatchLeading (ReqBuffer, "close");

                ELSIF MatchLeading (ReqBuffer, "Host:") THEN
                    Strings.Assign (ReqBuffer, sess^.Host);

                ELSIF MatchLeading (ReqBuffer, "If-Modified-Since:") THEN
                    Strings.Assign (ReqBuffer, sess^.ifmodifiedsince);

                END (*IF*);

            ELSE
                InHeader := FALSE;
            END (*IF*);

        END (*WHILE*);

    END ParseParameters;

(************************************************************************)

PROCEDURE HexVal (ch: CHAR): CARDINAL;

    (* Converts a hexadecimal character to numeric. *)

    TYPE CharSet = SET OF CHAR;

    BEGIN
        IF ch IN CharSet{'0'..'9'} THEN
            RETURN ORD(ch) - ORD('0');
        ELSIF ch IN CharSet{'A'..'F'} THEN
            RETURN ORD(ch) - ORD('A') + 10;
        ELSIF ch IN CharSet{'a'..'f'} THEN
            RETURN ORD(ch) - ORD('a') + 10;
        END (*IF*);
    END HexVal;

(************************************************************************)

PROCEDURE CopyURL (sess: Session;  VAR (*IN*) URL: ARRAY OF CHAR);

    (* Puts the URL into sess^.URL, with some special-case handling. *)

    VAR j, k, pos: CARDINAL;  ch: CHAR;  found: BOOLEAN;

    BEGIN
        Strings.Assign (URL, sess^.URL);

        (* Process any %NN hex codes in the URL. *)

        ch := sess^.URL[0];  j := 1;  k := 0;
        WHILE ch <> Nul DO
            IF ch = '%' THEN
                ch := CHR(16*HexVal(sess^.URL[j]) + HexVal(sess^.URL[j+1]));
                INC (j, 2);
            END (*IF*);
            sess^.URL[k] := ch;  INC(k);
            ch := sess^.URL[j];  INC(j);
        END (*WHILE*);
        sess^.URL[k] := Nul;

        (* If the URL is in absoluteURI format, extract the host field. *)
        (* This will override any Host: header field.                   *)

        IF MatchLeading (sess^.URL, "http://") THEN
            Strings.FindNext ('/', sess^.URL, 0, found, pos);
            IF found THEN
                Strings.Extract (sess^.URL, 0, pos, sess^.Host);
                Strings.Delete (sess^.URL, 0, pos+1);
            END (*IF*);
        END (*IF*);

    END CopyURL;

(************************************************************************)
(*                            REQUEST HANDLER                           *)
(************************************************************************)

PROCEDURE HandleRequest (sess: Session);

    VAR pos: CARDINAL;
        found: BOOLEAN;
        method: ARRAY [0..7] OF CHAR;
        URL: ARRAY [0..1023] OF CHAR;
        Work: ARRAY [0..511] OF CHAR;

    BEGIN
        found := TRUE;  pos := 0;
        Strings.Assign (sess^.ReqBuffer, Work);

        (* Clear obsolete information from the last request. *)

        sess^.URL[0] := Nul;
        sess^.lastmodified[0] := Nul;
        sess^.ifmodifiedsince[0] := Nul;

        (* Ignore empty lines, even though clients should not produce them. *)

        WHILE found AND (Work[0] = Nul) DO
            found := GetLine (sess^.instream, Work);
        END (*IF*);

        (* Extract the method and URL. *)

        IF found THEN
            Strings.FindNext (' ', Work, 0, found, pos);
        END (*IF*);
        IF found THEN
            Strings.Extract (Work, 0, pos, method);
            Strings.Delete (Work, 0, pos+1);
            Strings.FindNext (' ', Work, 0, found, pos);
            IF found THEN
                Strings.Extract (Work, 0, pos, URL);
                Strings.Delete (Work, 0, pos+1);
            END (*IF*);
        END (*IF*);

        IF found THEN

            (* Process the request parameters and URL. *)

            ParseParameters (sess);
            CopyURL (sess, URL);

            (* Remark: the method is case-sensitive. *)

            IF Strings.Equal (method, "GET") THEN
                HandleGETcommand (sess, TRUE);
            ELSIF Strings.Equal (method, "HEAD") THEN
                HandleGETcommand (sess, FALSE);
            ELSE
                NotImplementedResponse (sess);
            END (*IF*);
        ELSE
            FaultyRequestResponse (sess);
        END (*IF*);

    END HandleRequest;

(************************************************************************)
(*          THE EXTERNALLY CALLABLE REQUEST-HANDLING PROCEDURE          *)
(************************************************************************)

PROCEDURE HandleOneRequest (sess: Session;
                                VAR (*OUT*) mustclose: BOOLEAN): BOOLEAN;

    (* Deals with a request to the server. *)

    VAR message: ARRAY [0..511] OF CHAR;

    BEGIN
        mustclose := TRUE;
        IF GetLine (sess^.instream, sess^.ReqBuffer) THEN
            message := "< ";
            Strings.Append (sess^.ReqBuffer, message);
            LogTransaction (sess^.LogID, message);
            (*Signal (KeepAliveSemaphore);*)
            HandleRequest (sess);
            mustclose := sess^.mustclose;
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END HandleOneRequest;

(************************************************************************)
(*                   LOADING OR RELOADING INI DATA                      *)
(************************************************************************)

PROCEDURE ReloadLoggingINIData (INIname: ARRAY OF CHAR;  UseTNI: BOOLEAN): BOOLEAN;

    (* Loads that part of the INI data, for the Delivery module, that   *)
    (* can be updated "on the run" without restarting the server.       *)
    (* Returns the value of ExtraLogging.                               *)

    VAR hini: HINI;
        TransLevel: CARDINAL;
        TransLogName: FilenameString;
        SyslogHost: HostName;
        key: ARRAY [0..20] OF CHAR;
        SYSapp: ARRAY [0..4] OF CHAR;
        ExtraLogging: BOOLEAN;

    BEGIN
        SyslogHost := "";
        TransLogName := "WEBSERVE.LOG";
        TransLevel := 2;
        Obtain (CommonLogLock);
        hini := OpenINIFile (INIname, UseTNI);
        SYSapp := "$SYS";
        IF INIValid(hini) THEN
            key := "SyslogHost";
            EVAL (INIGetString (hini, SYSapp, key, SyslogHost));
            key := "TransLevel";
            IF NOT INIGet (hini, SYSapp, key, TransLevel) THEN
                TransLevel := 2;
            END (*IF*);
            key := "TransLogName";
            EVAL (INIGetString (hini, SYSapp, key, TransLogName));
            key := "CommonLogName";
            EVAL (INIGetString (hini, SYSapp, key, CommonLogName));
            key := "EnableCommonLog";
            IF NOT INIGet (hini, SYSapp, key, CommonLogEnabled) THEN
                CommonLogEnabled := FALSE;
            END (*IF*);
        END (*IF*);
        CloseINIFile (hini);
        Release (CommonLogLock);
        ExtraLogging := TransLevel > 15;
        TransLevel := TransLevel MOD 16;
        IF NOT NotDetached() THEN
            TransLevel := IAND(TransLevel, 5);
        END (*IF*);

        SetProcname ("Webserve", 2);
        IF SyslogHost[0] <> Nul THEN
            SetSyslogHost (SyslogHost);
        END (*IF*);
        StartTransactionLogging (LogCtx, TransLogName, TransLevel);

        RETURN ExtraLogging;

    END ReloadLoggingINIData;

(************************************************************************)

PROCEDURE LoadReqINIData (INIname: ARRAY OF CHAR;  UseTNI: BOOLEAN);

    (* Loads INI data needed to handle requests. *)

    BEGIN
        DetailedLogging := ReloadLoggingINIData(INIname, UseTNI);
        LoadMIMETable (INIname, UseTNI);
        UpdateDomainList (INIname, UseTNI);
    END LoadReqINIData;

(************************************************************************)
(*                         MODULE INITIALISATION                        *)
(************************************************************************)

PROCEDURE LContext(): LogContext;

    (* Returns the log context used for transaction logging. *)

    BEGIN
        RETURN LogCtx;
    END LContext;

(********************************************************************************)

BEGIN
    CRLF[0] := CR;  CRLF[1] := LF;
    DetailedLogging := FALSE;
    LogCtx := OpenLogContext();
    NextName := "00000000";
    CreateLock (NextNameLock);
    CreateLock (CommonLogLock);
FINALLY
    CloseLogContext (LogCtx);
    DestroyLock (CommonLogLock);
    DestroyLock (NextNameLock);
END Requests.

