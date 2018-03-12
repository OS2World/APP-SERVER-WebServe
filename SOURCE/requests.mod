(**************************************************************************)
(*                                                                        *)
(*  Web server session manager                                            *)
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

IMPLEMENTATION MODULE Requests;

        (********************************************************)
        (*                                                      *)
        (*            Handle requests to web server             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2015                    *)
        (*  Last edited:        10 March 2018                   *)
        (*  Status:             OK                              *)
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
(*      Handle If-Range header fields.                                  *)
(*      Handle "partial GET" requested by a Range header.               *)
(*                                                                      *)
(************************************************************************)

IMPORT Strings, OS2;

FROM SYSTEM IMPORT
    (* type *)  LOC,
    (* proc *)  CAST;

FROM Names IMPORT
    (* type *)  FilenameIndex, FilenameString, HostName;

FROM Arith64 IMPORT
    (* type *)  CARD64,
    (* const*)  Zero64,
    (* proc *)  IsZero, Compare64, Sub64;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM WSV IMPORT
    (* const*)  version;

FROM Sockets IMPORT
    (* type *)  Socket, SockAddr,
    (* proc *)  send;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, INIValid, CloseINIFile, INIGet, INIGetString;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached;

FROM TransLog IMPORT
    (* proc *)  OpenLogContext, CloseLogContext,
                StartTransactionLogging, SetProcname, SetSyslogHost;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM MiscFuncs IMPORT
    (* proc *)  StringMatch,
    (* proc *)  ConvertCard64, GetNum;

FROM Inet2Misc IMPORT
    (* proc *)  WaitForSocketOut;

FROM MyClock IMPORT
    (* proc *)  FormatCurrentDateTime, CurrentDateAndTimeGMT, CompareDateStrings;

FROM TransLog IMPORT
    (* type *)  LogContext, TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM InStream IMPORT
    (* type *)  IStream,
    (* proc *)  OpenIStream, CloseIStream, GetLine, GetBytes;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, DirectoryEntry,
    (* proc *)  OpenOldFile, OpenAtEnd, OpenNewFile, CloseFile, Exists,
                ReadRaw, ReadLine, WriteRaw,
                FWriteChar, FWriteString, FWriteLJCard64, FWriteLn,
                DeleteFile, FirstDirEntry, DirSearchDone;

FROM Misc IMPORT
    (* proc *)  MakeNewFilename, SkipLeadingSpaces, MatchLeading;

FROM Watchdog IMPORT
    (* type *)  WatchdogID,
    (* proc *)  KickWatchdog;

FROM SSI IMPORT
    (* proc *)  ProcessSSI;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  UpdateDomainList, OpenDomain, CloseDomain, LocateFile,
                SizeOf, DirOf;

FROM MIMEtypes IMPORT
    (* proc *)  IdentifyType;

FROM Environment IMPORT
    (* type *)  EnvRecordPtr,
    (* proc *)  AddToEnvironmentString, DiscardEnvironmentString;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);
    NilDomain = CAST(Domain, NIL);

VAR CRLF: ARRAY [0..1] OF CHAR;

TYPE
    Session = POINTER TO
                  RECORD
                      LogID: TransactionLogID;
                      socket: Socket;
                      watchID: WatchdogID;
                      instream: IStream;
                      ClientName: ARRAY [0..511] OF CHAR;

                      (* The remaining fields are specific  *)
                      (* to the current request.            *)

                      penv: EnvRecordPtr;
                      mustclose: BOOLEAN;
                      domain: Domain;
                      contentlength: CARDINAL;
                      ReqBuffer: ARRAY [0..1023] OF CHAR;
                      URL:  ARRAY [0..1023] OF CHAR;
                      Host: ARRAY [0..255] OF CHAR;
                      ifmodifiedsince: ARRAY [0..31] OF CHAR;

                  END (*RECORD*);

VAR
    (* Log context for the program. *)

    LogCtx: LogContext;

    (* Common log. *)

    CommonLogLock: Lock;
    CommonLogName: FilenameString;
    CommonLogEnabled: BOOLEAN;

    (* Specifies how much to put in the transaction log. *)

    DetailedLogging: BOOLEAN;

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

    VAR cid: ChanId;
        Buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Obtain (CommonLogLock);
        cid := OpenAtEnd (CommonLogName);
        FWriteString (cid, sess^.ClientName);
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

PROCEDURE PutFile (sess: Session;  id: ChanId);

    (* Sends from an already-opened file. *)

    CONST BufferSize = 65536;

    VAR BuffPtr: POINTER TO ARRAY [0..BufferSize-1] OF LOC;
        S: Socket;  xferred: CARDINAL;  success: BOOLEAN;

    BEGIN
        S := sess^.socket;
        NEW (BuffPtr);
        success := TRUE;
        LOOP
            ReadRaw (id, BuffPtr^, BufferSize, xferred);
            IF xferred = 0 THEN
                EXIT(*LOOP*)
            END (*IF*);
            KickWatchdog (sess^.watchID);
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

PROCEDURE SendFile (sess: Session;  VAR (*IN*) name: ARRAY OF CHAR);

    (* Sends file 'name' to sess^.socket. *)

    VAR cid: ChanId;

    BEGIN
        cid := OpenOldFile (name, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            PutFile (sess, cid);
            CloseFile (cid);
        END (*IF*);
    END SendFile;

(************************************************************************)
(*                   OPENING AND CLOSING A SESSION                      *)
(************************************************************************)

PROCEDURE OpenSession (S: Socket;  ID: TransactionLogID;
                            ClientName: ARRAY OF CHAR;
                            WID: WatchdogID): Session;

    (* Creates the session state for a new session. *)

    VAR result: Session;

    BEGIN
        NEW (result);
        result^.LogID := ID;
        result^.socket := S;
        Strings.Assign (ClientName, result^.ClientName);
        result^.watchID := WID;
        result^.instream := OpenIStream (S);
        result^.domain := NilDomain;
        result^.contentlength := 0;
        result^.mustclose := FALSE;
        result^.Host := "";
        result^.URL := "";
        result^.ReqBuffer := "";
        result^.ifmodifiedsince := "";
        result^.penv := NIL;
        AddToEnvironmentString (result^.penv, "REMOTE_HOST", ClientName);
        RETURN result;
    END OpenSession;

(************************************************************************)

PROCEDURE CloseSession (VAR (*INOUT*) sess: Session);

    (* Terminates a session. *)

    BEGIN
        IF sess <> NIL THEN
            CloseIStream (sess^.instream);
            CloseDomain (sess^.domain);
            DiscardEnvironmentString (sess^.penv);
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
        pos: CARDINAL;
        size: CARD64;  CGI, SHTML: BOOLEAN;
        message: ARRAY [0..63] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;
        lastmodified: ARRAY [0..31] OF CHAR;
        filename: FilenameString;

    BEGIN
        S := sess^.socket;
        ID := sess^.LogID;
        PutAndLogLine (S, ID, "HTTP/1.1 404 Not Found", TRUE);
        message := "Date: ";
        CurrentDateAndTimeGMT (parambuffer);
        Strings.Append (parambuffer, message);
        PutAndLogLine (S, ID, message, FALSE);
        message := "/404*.html";
        IF LocateFile (sess^.domain, message, filename,
                                  lastmodified, size, CGI, SHTML) THEN
            PutAndLogLine (S, ID, "Content-Type: text/html", FALSE);
            message := "Content-Length: ";
            pos := Strings.Length (message);
            ConvertCard64 (size, message, pos);
            message[pos] := Nul;
            PutAndLogLine (S, ID, message, FALSE);
            PutAndLogLine (S, ID, "", FALSE);

            SendFile (sess, filename);

        ELSE
            PutAndLogLine (S, ID, "Content-Type: text/plain", FALSE);
            PutAndLogLine (S, ID, "Content-Length: 15", FALSE);
            PutAndLogLine (S, ID, "", FALSE);
            PutAndLogLine (S, ID, "404 Not Found", FALSE);
            size := CARD64{15,0};
        END (*IF*);

        IF CommonLogEnabled THEN
            MakeCommonLogEntry (sess, "404", size);
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
(*                        EXECUTING A CGI SCRIPT                        *)
(************************************************************************)

PROCEDURE ExecProgram (sess: Session;  progname: ARRAY OF CHAR;
                            VAR (*IN*) tempfile: ARRAY OF CHAR);

    (* Executes a CGI application.  Progname specifies the program or   *)
    (* script to be executed, optionally followed by a '?' followed by  *)
    (* parameters to be sent to the program.  Program output is put in  *)
    (* tempfile, which the caller must delete after use.                *)

    CONST
        ONLength = 256;
        HF_STDIN = 0;
        HF_STDOUT = 1;
        PipeSize = 2048;

    VAR j, result, pos, actual, wanted, total, written: CARDINAL;
        ArgString, progparams: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        found: BOOLEAN;
        rc: OS2.APIRET;
        SaveStdOut, outRead, outWrite, StdOut,
            SaveStdIn, inRead, inWrite, StdIn: OS2.HFILE;
        cid: ChanId;
        Buffer: ARRAY [0..PipeSize-1] OF CHAR;

    BEGIN
        (* Dummy assignments to avoid a compiler warning. *)

        inWrite := 0;  inRead := 0;  SaveStdIn := 0;

        (* First separate out the parameters, if any, to be passed to   *)
        (* the CGI script or program.                                   *)

        Strings.FindNext ('?', progname, 0, found, pos);
        IF found THEN
            Strings.Extract (progname, pos+1, LENGTH(progname)-pos-1, progparams);
            progname[pos] := Nul;
            AddToEnvironmentString (sess^.penv, "QUERY_STRING", progparams);
        END (*IF*);

        (* Save the standard output handle, so that we can later restore it. *)

        rc := OS2.DosDupHandle (HF_STDOUT, SaveStdOut);

        (* Create a pipe for the script output, and map standard output *)
        (* to the "write" end of the pipe.                              *)

        rc := OS2.DosCreatePipe(outRead, outWrite, PipeSize);
        StdOut := HF_STDOUT;
        rc := OS2.DosDupHandle (outWrite, StdOut);

        (* Do the same for the standard input, if needed. *)

        IF sess^.contentlength > 0 THEN
            rc := OS2.DosDupHandle (HF_STDIN, SaveStdIn);
            rc := OS2.DosCreatePipe(inRead, inWrite, PipeSize);
            StdIn := HF_STDIN;
            rc := OS2.DosDupHandle (inRead, StdIn);
        END (*IF*);

        (* Set up the arguments for DosExecPgm. *)

        ArgString := "CMD /C ";
        Strings.Append (progname, ArgString);

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
                                  OS2.EXEC_ASYNC, ArgString, sess^.penv^.content,
                                  ExitStatus, "CMD.EXE");

        (* At present I'm not doing anything with the exit status.      *)
        (* Some more error checking is desirable here.                  *)

        wanted := sess^.contentlength;
        IF wanted > 0 THEN

            (* Copy from socket stream to input pipe. *)

            WHILE wanted > 0 DO
                GetBytes (sess^.instream, Buffer, wanted, actual);
                IF actual = 0 THEN
                    wanted := 0;        (* to force loop termination *)
                ELSE
                    total := 0;
                    REPEAT
                        rc := OS2.DosWrite (inWrite, Buffer, actual, written);
                        INC (total, written);
                    UNTIL written = actual;
                    DEC (wanted, actual);
                END (*IF*);
            END (*LOOP*);

            OS2.DosClose (inWrite);
            OS2.DosDupHandle (SaveStdIn, StdIn);

        END (*IF*);

        (* Here I would have expected to wait for the child process to  *)
        (* terminate, but for some reason a DosWaitChild never returns. *)
        (* Perhaps the DosClose is sufficient.                          *)

        (* Close write handle of the output pipe. *)

        OS2.DosClose (outWrite);

        (* Restore the original stdout. *)

        OS2.DosDupHandle (SaveStdOut, StdOut);

        (* Copy from pipe to temporary file. *)

        cid := OpenNewFile (tempfile, FALSE);
        LOOP
            rc := OS2.DosRead (outRead, Buffer, PipeSize, actual);
            IF (rc = OS2.ERROR_NO_DATA) OR (actual = 0) THEN
                EXIT (*LOOP*);
            END (*IF*);
            WriteRaw (cid, Buffer, actual);
        END (*LOOP*);

        CloseFile (cid);

    END ExecProgram;

(************************************************************************)
(*                 HANDLING THE GET AND HEAD COMMANDS                   *)
(************************************************************************)

PROCEDURE HandleGETcommand (sess: Session;  IncludeBody: BOOLEAN);

    VAR message: ARRAY [0..1023] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;
        lastmodified: ARRAY [0..31] OF CHAR;
        filename, tempfile: FilenameString;
        S: Socket;  CGI, SHTML, dontsend, mustdelete: BOOLEAN;
        pos: CARDINAL;
        ID: TransactionLogID;
        size: CARD64;

    BEGIN
        ID := sess^.LogID;
        IF sess^.domain = NilDomain THEN
            sess^.domain := OpenDomain (sess^.Host);
        END (*IF*);
        S := sess^.socket;
        mustdelete := FALSE;

        IF sess^.domain = NilDomain THEN

            NotFoundResponse (sess);

        ELSIF LocateFile (sess^.domain, sess^.URL, filename,
                                  lastmodified, size, CGI, SHTML) THEN

            dontsend := FALSE;
            IF NOT CGI AND NOT SHTML AND (sess^.ifmodifiedsince[0] <> Nul) THEN
                dontsend := CompareDateStrings (lastmodified,
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
                    ExecProgram (sess, filename, tempfile);
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

                        SendFile (sess, tempfile);
                    ELSE
                        PutAndLogLine (S, ID, "", FALSE);
                        size := Zero64;
                    END (*IF*);
                    DeleteFile (tempfile);

                ELSE
                    (* Return the file. *)

                    IdentifyType (filename, parambuffer);
                    IF SHTML THEN
                        DirOf (sess^.domain, filename, tempfile);
                        ProcessSSI (sess, sess^.domain, filename, tempfile);
                        lastmodified[0] := Nul;
                        size := SizeOf (filename);
                        mustdelete := TRUE;
                    END (*IF*);
                    IF parambuffer[0] <> Nul THEN
                        message := "Content-Type: ";
                        Strings.Append (parambuffer, message);
                        PutAndLogLine (S, ID, message, FALSE);
                    END (*IF*);

                    IF lastmodified[0] <> Nul THEN
                        message := "Last-Modified: ";
                        Strings.Append (lastmodified, message);
                        PutAndLogLine (S, ID, message, FALSE);
                    END (*IF*);

                    message := "Content-Length: ";
                    pos := Strings.Length (message);
                    ConvertCard64 (size, message, pos);
                    message[pos] := Nul;
                    PutAndLogLine (S, ID, message, FALSE);

                    PutAndLogLine (S, ID, "", FALSE);

                    IF IncludeBody THEN
                        SendFile (sess, filename);
                        IF mustdelete THEN
                            DeleteFile (filename);
                        END (*IF*);
                    ELSE
                        size := Zero64;
                    END (*IF*);

                END (*IF CGI*);

                IF CommonLogEnabled THEN
                    MakeCommonLogEntry (sess, "200", size);
                END (*IF*);

            END (*IF dontsend*);

        ELSE
            (* File not found, cannot satisfy request. *)

            NotFoundResponse (sess);

        END (*IF*);

    END HandleGETcommand;

(************************************************************************)
(*                     HANDLING THE POST COMMAND                        *)
(************************************************************************)

PROCEDURE HandlePOSTcommand (sess: Session);

    VAR message: ARRAY [0..255] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;
        lastmodified: ARRAY [0..31] OF CHAR;
        S: Socket;  CGI, SHTML: BOOLEAN;
        pos: CARDINAL;
        ID: TransactionLogID;
        size: CARD64;
        filename, tempfile: FilenameString;

    BEGIN
        ID := sess^.LogID;
        IF sess^.domain = NilDomain THEN
            sess^.domain := OpenDomain (sess^.Host);
        END (*IF*);
        S := sess^.socket;

        IF sess^.domain = NilDomain THEN

            NotFoundResponse (sess);

        ELSIF LocateFile (sess^.domain, sess^.URL, filename,
                                  lastmodified, size, CGI, SHTML) THEN

            IF CGI THEN

                (* General "OK" header lines. *)

                PutAndLogLine (S, ID, "HTTP/1.1 200 OK", TRUE);

                message := "Server: WebServe/";
                Strings.Append (version, message);
                PutAndLogLine (S, ID, message, FALSE);

                message := "Date: ";
                CurrentDateAndTimeGMT (parambuffer);
                Strings.Append (parambuffer, message);
                PutAndLogLine (S, ID, message, FALSE);

                IF sess^.mustclose THEN
                    PutAndLogLine (S, ID, "Connection: close", FALSE);
                ELSE
                    PutAndLogLine (S, ID, "Connection: keep-alive", FALSE);
                END (*IF*);

                (* Execute the CGI application. *)

                MakeNewFilename (tempfile);
                ExecProgram (sess, filename, tempfile);
                size := BodySize (tempfile);
                message := "Content-Length: ";
                pos := Strings.Length (message);
                ConvertCard64 (size, message, pos);
                message[pos] := Nul;
                PutAndLogLine (S, ID, message, FALSE);

                IF IsZero(size)  THEN
                    PutAndLogLine (S, ID, "", FALSE);
                ELSE

                    (* We expect that tempfile also includes some   *)
                    (* header lines, plus the blank line that       *)
                    (* terminates the header.                       *)

                    SendFile (sess, tempfile);
                END (*IF*);
                DeleteFile (tempfile);

            ELSE
                NotImplementedResponse (sess);
            END (*IF*);

        ELSE
            (* File not found, cannot satisfy request. *)

            NotFoundResponse (sess);

        END (*IF*);

    END HandlePOSTcommand;

(************************************************************************)
(*                  PARSING THE HEADER OF A REQUEST                     *)
(************************************************************************)

PROCEDURE ParseParameters (sess: Session);

    VAR ReqBuffer, message: ARRAY [0..511] OF CHAR;
        acceptlanguage: ARRAY [0..255] OF CHAR;
        pos: CARDINAL;
        InHeader: BOOLEAN;

    BEGIN
        (* Clear obsolete information from the last request. *)

        sess^.URL[0] := Nul;
        sess^.Host[0] := Nul;
        CloseDomain (sess^.domain);
        sess^.contentlength := 0;
        sess^.ifmodifiedsince[0] := Nul;
        acceptlanguage := "en";
        DiscardEnvironmentString (sess^.penv);

        (* Get new values from header. *)

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
                    Strings.Assign (ReqBuffer, acceptlanguage);

                ELSIF MatchLeading (ReqBuffer, "Content-Length:") THEN
                    SkipLeadingSpaces (ReqBuffer);
                    pos := 0;
                    sess^.contentlength := GetNum (ReqBuffer, pos);
                    AddToEnvironmentString (sess^.penv, "CONTENT_LENGTH", ReqBuffer);

                ELSIF MatchLeading (ReqBuffer, "Content-Type:") THEN
                    SkipLeadingSpaces (ReqBuffer);
                    AddToEnvironmentString (sess^.penv, "CONTENT_TYPE", ReqBuffer);

                ELSIF MatchLeading (ReqBuffer, "Connection:") THEN
                    SkipLeadingSpaces (ReqBuffer);
                    sess^.mustclose := MatchLeading (ReqBuffer, "close");

                ELSIF MatchLeading (ReqBuffer, "Host:") THEN
                    IF sess^.Host[0] = Nul THEN
                        Strings.Assign (ReqBuffer, sess^.Host);
                    END (*IF*);

                ELSIF MatchLeading (ReqBuffer, "If-Modified-Since:") THEN
                    Strings.Assign (ReqBuffer, sess^.ifmodifiedsince);

                END (*IF*);

            ELSE
                InHeader := FALSE;
            END (*IF*);

        END (*WHILE*);

        (* We can now set environment variables that might or might not *)
        (* have been supplied in the header. *)

        AddToEnvironmentString (sess^.penv, "HTTP_ACCEPT_LANGUAGE", acceptlanguage);

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

        (* Ignore empty lines, even though clients should not produce them. *)

        WHILE found AND (Work[0] = Nul) DO
            found := GetLine (sess^.instream, Work);
        END (*IF*);

        IF NOT found THEN

            (* End of input. *)

            sess^.mustclose := TRUE;
            RETURN;
        END (*IF*);

        (* Extract the method and URL. *)

        Strings.FindNext (' ', Work, 0, found, pos);
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

            AddToEnvironmentString (sess^.penv, "REQUEST_METHOD", method);
            IF Strings.Equal (method, "GET") THEN
                HandleGETcommand (sess, TRUE);
            ELSIF Strings.Equal (method, "HEAD") THEN
                HandleGETcommand (sess, FALSE);
            ELSIF Strings.Equal (method, "POST") THEN
                HandlePOSTcommand (sess);
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
    CreateLock (CommonLogLock);
FINALLY
    CloseLogContext (LogCtx);
    DestroyLock (CommonLogLock);
END Requests.

