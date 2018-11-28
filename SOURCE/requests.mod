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
        (*  Last edited:        11 November 2018                *)
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
    (* type *)  Socket;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, INIValid, CloseINIFile, INIGet, INIGetString;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached;

FROM TransLog IMPORT
    (* proc *)  OpenLogContext, CloseLogContext,
                StartTransactionLogging, SetProcname, SetSyslogHost;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM MiscFuncs IMPORT
    (* proc *)  StringMatch,
    (* proc *)  ConvertCard, ConvertCard64, GetNum;

FROM MyClock IMPORT
    (* proc *)  FormatCurrentDateTime, CurrentDateAndTimeGMT, CompareDateStrings;

FROM TransLog IMPORT
    (* type *)  LogContext, TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM NetStream IMPORT
    (* type *)  NStream,
    (* proc *)  OpenNetStream, CloseNetStream, GetLine, GetBytes,
                PutBytes, PutEOL;

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
                      watchID: WatchdogID;
                      sockstream: NStream;
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
            (*LogTransactionL (sess^.LogID, "file not found");*)
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
(*                        OUTPUT TO SOCKET STREAM                       *)
(************************************************************************)

PROCEDURE PutAndLogLine (NS: NStream;  LogID: TransactionLogID;
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
            EVAL (PutBytes (NS, text, LENGTH(text)));
        END (*IF*);
        PutEOL (NS);
    END PutAndLogLine;

(************************************************************************)

PROCEDURE PutFile (sess: Session;  id: ChanId);

    (* Sends from an already-opened file. *)

    CONST BufferSize = 65536;

    VAR BuffPtr: POINTER TO ARRAY [0..BufferSize-1] OF LOC;
        NS: NStream;  xferred: CARDINAL;  success: BOOLEAN;

    BEGIN
        NS := sess^.sockstream;
        NEW (BuffPtr);
        success := TRUE;
        LOOP
            ReadRaw (id, BuffPtr^, BufferSize, xferred);
            IF xferred = 0 THEN
                EXIT(*LOOP*)
            END (*IF*);
            KickWatchdog (sess^.watchID);
            IF xferred = MAX(CARDINAL) THEN
                success := FALSE;
            END (*IF*);
            IF NOT success THEN
                EXIT(*LOOP*)
            END (*IF*);
            EVAL (PutBytes (NS, BuffPtr^, xferred));
        END (*LOOP*);
        DISPOSE (BuffPtr);
    END PutFile;

(************************************************************************)

PROCEDURE SendFile (sess: Session;  VAR (*IN*) name: ARRAY OF CHAR);

    (* Sends file 'name' to sess^.sockstream. *)

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

PROCEDURE OpenSession (NS: NStream;  ID: TransactionLogID;
                            ClientName: ARRAY OF CHAR;
                            WID: WatchdogID): Session;

    (* Creates the session state for a new session. *)

    CONST
        IsServer = TRUE;
        useTLS = FALSE;         (* until I get TLS working *)

    VAR result: Session;

    BEGIN
        NEW (result);
        result^.LogID := ID;
        Strings.Assign (ClientName, result^.ClientName);
        result^.watchID := WID;
        result^.sockstream := NS;
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
            CloseNetStream (sess^.sockstream);
            CloseDomain (sess^.domain);
            DiscardEnvironmentString (sess^.penv);
            DISPOSE (sess);
        END (*IF*);
    END CloseSession;

(************************************************************************)
(*                     RESPONSE FOR FAILURE CASES                       *)
(************************************************************************)

PROCEDURE FaultyRequestResponse (sess: Session);

    VAR NS: NStream;

    BEGIN
        NS := sess^.sockstream;
        PutAndLogLine (NS, sess^.LogID, "HTTP/1.1 400 Bad Request", TRUE);
        PutAndLogLine (NS, sess^.LogID, "", FALSE);
        IF CommonLogEnabled THEN
            MakeCommonLogEntry (sess, "400", Zero64);
        END (*IF*);
    END FaultyRequestResponse;

(************************************************************************)

PROCEDURE NotFoundResponse (sess: Session);

    VAR ID: TransactionLogID;
        NS: NStream;
        pos: CARDINAL;
        size: CARD64;  CGI, SHTML: BOOLEAN;
        message: ARRAY [0..63] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;
        lastmodified: ARRAY [0..31] OF CHAR;
        filename: FilenameString;

    BEGIN
        NS := sess^.sockstream;
        ID := sess^.LogID;
        PutAndLogLine (NS, ID, "HTTP/1.1 404 Not Found", TRUE);
        message := "Date: ";
        CurrentDateAndTimeGMT (parambuffer);
        Strings.Append (parambuffer, message);
        PutAndLogLine (NS, ID, message, FALSE);
        message := "/404*.html";
        IF LocateFile (sess^.domain, message, filename,
                                  lastmodified, size, CGI, SHTML) THEN
            PutAndLogLine (NS, ID, "Content-Type: text/html", FALSE);
            message := "Content-Length: ";
            pos := Strings.Length (message);
            ConvertCard64 (size, message, pos);
            message[pos] := Nul;
            PutAndLogLine (NS, ID, message, FALSE);
            PutAndLogLine (NS, ID, "", FALSE);

            SendFile (sess, filename);

        ELSE
            PutAndLogLine (NS, ID, "Content-Type: text/plain", FALSE);
            PutAndLogLine (NS, ID, "Content-Length: 15", FALSE);
            PutAndLogLine (NS, ID, "", FALSE);
            PutAndLogLine (NS, ID, "404 Not Found", FALSE);
            size := CARD64{15,0};
        END (*IF*);

        IF CommonLogEnabled THEN
            MakeCommonLogEntry (sess, "404", size);
        END (*IF*);

    END NotFoundResponse;

(************************************************************************)

PROCEDURE NotImplementedResponse (sess: Session);

    VAR NS: NStream;

    BEGIN
        NS := sess^.sockstream;
        PutAndLogLine (NS, sess^.LogID, "HTTP/1.1 501 Not Implemented", TRUE);
        PutAndLogLine (NS, sess^.LogID, "", FALSE);
        IF CommonLogEnabled THEN
            MakeCommonLogEntry (sess, "501", Zero64);
        END (*IF*);
    END NotImplementedResponse;

(************************************************************************)
(*                        EXECUTING A CGI SCRIPT                        *)
(************************************************************************)

PROCEDURE ExecProgram (sess: Session;  progname, outfile: ARRAY OF CHAR);

    (* Executes a CGI application.  Progname specifies the program or   *)
    (* script to be executed, optionally followed by a '?' followed by  *)
    (* parameters to be sent to the program.  If outfile is a nonempty  *)
    (* string then we create a file with that name and write program    *)
    (* output to that file.  Otherwise program output is sent to the    *)
    (* socket for this session.                                         *)

    CONST
        ONLength = 256;
        HF_STDIN = 0;
        HF_STDOUT = 1;
        PipeSize = 65536;
        outbufSize = 32768;

    VAR j, result, pos, actual, inputlength, total, written: CARDINAL;
        pid: OS2.PID;
        cid: ChanId;
        ArgString, progparams: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        useoutfile, found, PipeInput: BOOLEAN;
        rc: OS2.APIRET;
        SaveStdIn, inRead, inWrite, StdIn: OS2.HFILE;
        SaveStdOut, outRead, outWrite, StdOut: OS2.HFILE;
        bufptr: POINTER TO ARRAY [0..PipeSize-1] OF CHAR;
        outbufptr: POINTER TO ARRAY [0..outbufSize-1] OF CHAR;

    BEGIN
        (* Dummy assignments to avoid a compiler warning. *)

        inWrite := 0;  inRead := 0;  SaveStdIn := MAX(CARDINAL);
        outWrite := 0;  outRead := 0;  SaveStdOut := MAX(CARDINAL);
        bufptr := NIL;  outbufptr := NIL;
        cid := NoSuchChannel;
        useoutfile := outfile[0] <> Nul;

        (* First separate out the parameters, if any, to be passed to   *)
        (* the CGI script or program.                                   *)

        Strings.FindNext ('?', progname, 0, found, pos);
        IF found THEN
            Strings.Extract (progname, pos+1, LENGTH(progname)-pos-1, progparams);
            progname[pos] := Nul;
            AddToEnvironmentString (sess^.penv, "QUERY_STRING", progparams);
        END (*IF*);

        (* If we have some data to send to the script (a CGI POST),     *)
        (* save the standard input handle, so that we can later         *)
        (* restore it, and then create a pipe that feeds the data to    *)
        (* what the script will see as standard input.                  *)

        inputlength := sess^.contentlength;
        PipeInput := inputlength > 0;
        IF PipeInput THEN
            rc := OS2.DosDupHandle (HF_STDIN, SaveStdIn);
            rc := OS2.DosCreatePipe(inRead, inWrite, PipeSize);
            StdIn := HF_STDIN;
            rc := OS2.DosDupHandle (inRead, StdIn);
        END (*IF*);

        (* Similarly, arrange to pipe standard output. *)

        rc := OS2.DosDupHandle (HF_STDOUT, SaveStdOut);
        rc := OS2.DosCreatePipe(outRead, outWrite, PipeSize);
        StdOut := HF_STDOUT;
        rc := OS2.DosDupHandle (outWrite, StdOut);

        (* Set up the arguments for DosExecPgm.  My experiments suggest *)
        (* that piping the script output is slow, and that we're better *)
        (* off using the command processor's '>' piping feature.        *)

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

        (* Note: EXEC_ASYNC is needed if we have to pipe input to the   *)
        (* script, because otherwise the script would wait forever for  *)
        (* its input.  My tests appear to show that it is also needed   *)
        (* if we are piping output from the script, even though I can't *)
        (* think of any reason why that should be needed.               *)

        (* My original intention was to spawn an extra thread to handle *)
        (* the piped input and output, but now that I've settled on     *)
        (* using asynchronous execution of the script the present code  *)
        (* is effectively a separate thread anyway.                     *)

        IF PipeInput THEN

            (* Copy from socket stream to input pipe. *)

            NEW (bufptr);
            WHILE inputlength > 0 DO
                GetBytes (sess^.sockstream, bufptr^, inputlength, actual);
                IF actual = 0 THEN
                    inputlength := 0;        (* to force loop termination *)
                ELSE
                    total := 0;
                    REPEAT
                        rc := OS2.DosWrite (inWrite, bufptr^, actual, written);
                        INC (total, written);
                    UNTIL written = actual;
                    DEC (inputlength, actual);
                END (*IF*);
            END (*WHILE*);

            OS2.DosClose (inWrite);
            OS2.DosDupHandle (SaveStdIn, StdIn);

        END (*IF*);

        IF bufptr <> NIL THEN
            DISPOSE (bufptr);
        END (*IF*);

        (* Now deal with the piped output. *)

        (* Close the "write" end of the output pipe, and restore standard output. *)

        OS2.DosClose (outWrite);
        OS2.DosDupHandle (SaveStdOut, StdOut);

        (* Copy from output pipe to socket stream. *)

        NEW (outbufptr);
        IF useoutfile THEN
            cid := OpenNewFile (outfile, FALSE);
        END (*IF*);
        REPEAT
            IF OS2.DosRead (outRead, outbufptr^,
                            outbufSize, actual)= OS2.ERROR_NO_DATA THEN
                actual := 0;
            END (*IF*);
            IF actual > 0 THEN
                IF useoutfile THEN
                    WriteRaw (cid, outbufptr^, actual);
                ELSE
                    actual := PutBytes (sess^.sockstream, outbufptr^, actual);
                    IF actual = MAX(CARDINAL) THEN
                        actual := 0;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
        UNTIL actual = 0;
        OS2.DosClose (outRead);
        IF useoutfile THEN
            CloseFile (cid);
        END (*IF*);

        IF outbufptr <> NIL THEN
            DISPOSE (outbufptr);
        END (*IF*);

        (* DosWaitChild is needed after an asynchronous invocation, *)
        (* but not otherwise.                                       *)

        rc := OS2.DosWaitChild (0, 0, ExitStatus, pid, 0);

    END ExecProgram;

(************************************************************************)
(*                 HANDLING THE GET AND HEAD COMMANDS                   *)
(************************************************************************)

PROCEDURE HandleGETcommand (sess: Session;  IncludeBody: BOOLEAN);

    VAR message: ARRAY [0..1023] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;
        lastmodified: ARRAY [0..31] OF CHAR;
        filename, tempfile: FilenameString;
        NS: NStream;
        CGI, SHTML, dontsend, mustdelete: BOOLEAN;
        pos: CARDINAL;
        ID: TransactionLogID;
        size: CARD64;

    BEGIN
        ID := sess^.LogID;
        IF sess^.domain = NilDomain THEN
            sess^.domain := OpenDomain (sess^.Host);
        END (*IF*);
        NS := sess^.sockstream;
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
                PutAndLogLine (NS, ID, "HTTP/1.1 304 Not Modified", TRUE);
                PutAndLogLine (NS, ID, "", FALSE);
                IF CommonLogEnabled THEN
                    MakeCommonLogEntry (sess, "304", Zero64);
                END (*IF*);
            ELSE
                PutAndLogLine (NS, ID, "HTTP/1.1 200 Document follows", TRUE);

                message := "Server: WebServe/";
                Strings.Append (version, message);
                PutAndLogLine (NS, ID, message, FALSE);

                message := "Date: ";
                CurrentDateAndTimeGMT (parambuffer);
                Strings.Append (parambuffer, message);
                PutAndLogLine (NS, ID, message, FALSE);

                PutAndLogLine (NS, ID, "Accept-Ranges: none", FALSE);
                IF CGI THEN
                    sess^.mustclose := TRUE;
                END (*IF*);
                IF sess^.mustclose THEN
                    PutAndLogLine (NS, ID, "Connection: close", FALSE);
                ELSE
                    PutAndLogLine (NS, ID, "Connection: keep-alive", FALSE);
                END (*IF*);

                IF CGI THEN
                    (* Execute CGI application. *)

                    ExecProgram (sess, filename, "");

                    IF NOT IncludeBody THEN
                        PutAndLogLine (NS, ID, "", FALSE);
                        size := Zero64;
                    END (*IF*);

                ELSE  (* Not CGI *)

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
                        PutAndLogLine (NS, ID, message, FALSE);
                    END (*IF*);

                    IF lastmodified[0] <> Nul THEN
                        message := "Last-Modified: ";
                        Strings.Append (lastmodified, message);
                        PutAndLogLine (NS, ID, message, FALSE);
                    END (*IF*);

                    message := "Content-Length: ";
                    pos := Strings.Length (message);
                    ConvertCard64 (size, message, pos);
                    message[pos] := Nul;
                    PutAndLogLine (NS, ID, message, FALSE);

                    PutAndLogLine (NS, ID, "", FALSE);

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

    CONST usetempfile = TRUE;
            (* Until such time as I've revised this procedure. *)

    VAR message: ARRAY [0..255] OF CHAR;
        parambuffer: ARRAY [0..127] OF CHAR;
        lastmodified: ARRAY [0..31] OF CHAR;
        NS: NStream;
        CGI, SHTML: BOOLEAN;
        pos: CARDINAL;
        ID: TransactionLogID;
        size: CARD64;
        filename, tempfile: FilenameString;

    BEGIN
        ID := sess^.LogID;
        IF sess^.domain = NilDomain THEN
            sess^.domain := OpenDomain (sess^.Host);
        END (*IF*);
        NS := sess^.sockstream;

        IF sess^.domain = NilDomain THEN

            NotFoundResponse (sess);

        ELSIF LocateFile (sess^.domain, sess^.URL, filename,
                                  lastmodified, size, CGI, SHTML) THEN

            IF CGI THEN

                (* General "OK" header lines. *)

                PutAndLogLine (NS, ID, "HTTP/1.1 200 OK", TRUE);

                message := "Server: WebServe/";
                Strings.Append (version, message);
                PutAndLogLine (NS, ID, message, FALSE);

                message := "Date: ";
                CurrentDateAndTimeGMT (parambuffer);
                Strings.Append (parambuffer, message);
                PutAndLogLine (NS, ID, message, FALSE);

                IF sess^.mustclose THEN
                    PutAndLogLine (NS, ID, "Connection: close", FALSE);
                ELSE
                    PutAndLogLine (NS, ID, "Connection: keep-alive", FALSE);
                END (*IF*);

                (* Execute the CGI application. *)

                MakeNewFilename (tempfile);
                ExecProgram (sess, filename, tempfile);
                size := BodySize (tempfile);
                message := "Content-Length: ";
                pos := Strings.Length (message);
                ConvertCard64 (size, message, pos);
                message[pos] := Nul;
                PutAndLogLine (NS, ID, message, FALSE);

                IF IsZero(size)  THEN
                    PutAndLogLine (NS, ID, "", FALSE);
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
            IF GetLine (sess^.sockstream, ReqBuffer) THEN
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
            found := GetLine (sess^.sockstream, Work);
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
        IF GetLine (sess^.sockstream, sess^.ReqBuffer) THEN
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

