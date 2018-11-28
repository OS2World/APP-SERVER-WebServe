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

IMPLEMENTATION MODULE WSession;

        (********************************************************)
        (*                                                      *)
        (*           Session handler for web server             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2015                    *)
        (*  Last edited:        13 November 2018                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

FROM OS2 IMPORT
    (* const*)  FERR_DISABLEHARDERR,
    (* proc *)  DosError;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, SockAddr,
    (* proc *)  soclose, so_cancel, sock_errno;

FROM NetStream IMPORT
    (* type *)  NStream,
    (* proc *)  OpenNetStream;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release,
                NotDetached, CreateTask1, TaskExit;

FROM NameLookup IMPORT
    (* proc *)  StartNameLookup, GetName, CancelNameLookup;

FROM Timer IMPORT
    (* proc *)  Sleep, TimedWaitSpecial;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertCardRJ, AppendCard;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  StartTransactionLogging,
                CreateLogID, DiscardLogID, LogTransaction, LogTransactionL,
                UpdateTopScreenLine;

FROM Watchdog IMPORT
    (* type *)  WatchdogID,
    (* proc *)  AddToWatches, KickWatchdog, RemoveFromWatches;

FROM Requests IMPORT
    (* type *)  Session,
    (* proc *)  LContext, OpenSession, CloseSession, HandleOneRequest;

(************************************************************************)

CONST
    SessionPriority = 3;
    NilLogID = CAST(TransactionLogID, NIL);
    NilSession = CAST(Session, NIL);

TYPE
    (* String form of a session identifier. *)

    SevenChar = ARRAY [0..7] OF CHAR;

    (* Data used in creating a new instance of the session handler task. *)

    NewSessionPointer = POINTER TO
                           RECORD
                               socket: Socket;
                               IPAddress: CARDINAL;
                               secure: BOOLEAN;
                           END (*RECORD*);

    (* Data needed in timeout processing. *)

    SocketStatePointer = POINTER TO SocketStateRecord;
    SocketStateRecord = RECORD
                            ID: WatchdogID;
                            socket: Socket;
                            SocketOpen, TimedOut: BOOLEAN;
                        END (*RECORD*);

CONST
    Nul = CHR(0);

VAR
    (* Timeout delay, in milliseconds, and its mutual exclusion lock. *)

    MaxTime: CARDINAL;
    MaxTimeLock: Lock;

    (* Maximum allowed number of simultaneous clients.  For critical *)
    (* section protection we use lock ClientCountLock, see below.    *)

    MaxClients: CARDINAL;

    (* Count of active clients, and a lock to protect it. *)

    ClientCount: CARDINAL;
    ClientCountLock: Lock;

    (* Flag to control whether we write to the screen. *)

    LogToScreen: BOOLEAN;

    (* Option to show hostnames instead of IP addresses in the logs. *)

    ResolveIP: BOOLEAN;

(************************************************************************)
(*                 CLOSING A SOCKET, WITH ERROR CHECK                   *)
(************************************************************************)

PROCEDURE CloseSocket (S: Socket;  LogID: TransactionLogID);

    (* Closes socket S, logs an error message if the close fails. *)

    VAR code: CARDINAL;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        IF soclose(S) THEN
            code := sock_errno();
            Strings.Assign ("soclose failed, error code ", message);
            AppendCard (code, message);
            LogTransaction (LogID, message);
        END (*IF*);
    END CloseSocket;

(************************************************************************)
(*                UPDATING THE COUNT OF ACTIVE SESSIONS                 *)
(************************************************************************)

PROCEDURE UpdateCount (increment: INTEGER): CARDINAL;

    (* Updates the count of the number of clients, and returns the new  *)
    (* count.  Special case: if this would take us beyond the           *)
    (* MaxClients limit, then the count is not updated and the returned *)
    (* value is zero.                                                   *)

    VAR value, pos: CARDINAL;  Buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        Obtain (ClientCountLock);
        IF increment > 0 THEN INC (ClientCount, increment);
        ELSIF increment < 0 THEN DEC (ClientCount, -increment)
        END (*IF*);
        value := ClientCount;
        IF (value > MaxClients) AND (increment > 0) THEN
            DEC (ClientCount, increment);  value := 0;
        ELSIF LogToScreen THEN
            pos := 0;
            ConvertCardRJ (value, Buffer, 6, pos);
            Buffer[pos] := ' ';  INC(pos);
            Buffer[pos] := CHR(0);
            UpdateTopScreenLine (64, Buffer);
        END (*IF*);
        Release (ClientCountLock);
        RETURN value;
    END UpdateCount;

(************************************************************************)

PROCEDURE NumberOfClients(): CARDINAL;

    (* Returns the number of clients who are currently logged on. *)

    BEGIN
        RETURN UpdateCount(0);
    END NumberOfClients;

(************************************************************************)
(*                          THE TIMEOUT HANDLER                         *)
(************************************************************************)

PROCEDURE TimeoutHandler (arg: ADDRESS);

    (* This is called from the Watchdog module, which is telling us     *)
    (* that the session specified by arg has timed out.  We respond by  *)
    (* cancelling the main socket for this session.  That will cause a  *)
    (* failure of the input operation in the SessionHandler task, at    *)
    (* which point that task will discover the TimedOut flag is set.    *)

    VAR q: SocketStatePointer;

    BEGIN
        q := arg;
        q^.TimedOut := TRUE;
        IF q^.SocketOpen THEN
            so_cancel (q^.socket);
            q^.socket := NotASocket;
            q^.SocketOpen := FALSE;
        END (*IF*);

        (* Do not dispose of the q^ record, because the associated  *)
        (* service thread will want to inspect q^.TimedOut.         *)

    END TimeoutHandler;

(************************************************************************)
(*                       CLIENT SESSION HANDLER                         *)
(************************************************************************)

PROCEDURE SessionHandler (arg: ADDRESS);

    (* The task that handles a client session, i.e. this is where all   *)
    (* the real work is done.  There might be several instances of this *)
    (* task running, one for each session that is still open.           *)

    VAR S: Socket;
        LogID: TransactionLogID;
        ClientNumber: CARDINAL;
        sess: Session;
        KB: SocketStatePointer;

    (********************************************************************)

    PROCEDURE AbandonSession;

        (* Release resources and exit on session close.  It's less      *)
        (* error-prone to handle the resource release at a single point.*)

        BEGIN
            IF sess <> NilSession THEN
                CloseSession (sess);
            END (*IF*);
            IF KB <> NIL THEN
                RemoveFromWatches (KB^.ID, TRUE);
                DISPOSE (KB);
            END (*IF*);
            IF S <> NotASocket THEN
                EVAL(soclose(S));
            END (*IF*);
            IF ClientNumber <> 0 THEN
                EVAL (UpdateCount (-1));
            END (*IF*);
            IF LogID <> NilLogID THEN
                DiscardLogID (LogID);
            END (*IF*);
            TaskExit;
        END AbandonSession;

    (********************************************************************)

    VAR NSP: NewSessionPointer;
        IPAddress, T: CARDINAL;
        NameBuffer: ARRAY [0..511] OF CHAR;
        SessionNumber: SevenChar;
        LogMessage: ARRAY [0..127] OF CHAR;
        EndSession, success, useTLS: BOOLEAN;
        NS: NStream;

    BEGIN
        DosError (FERR_DISABLEHARDERR);
        NSP := arg;
        S := NSP^.socket;
        IPAddress := NSP^.IPAddress;
        useTLS := NSP^.secure;
        DISPOSE (NSP);

        sess := NIL;
        KB := NIL;

        (* Create the log file ID for this session. *)

        CardinalToString (S, SessionNumber, 7);  SessionNumber[7] := CHR(0);
        LogID := CreateLogID (LContext(), SessionNumber);

        (* Initial transaction log message. *)

        NameBuffer[0] := Nul;
        IF ResolveIP THEN
            GetName (IPAddress, NameBuffer, TRUE);
        END (*IF*);
        IF NameBuffer[0] = Nul THEN
            IPToString (IPAddress, TRUE, NameBuffer);
        END (*IF*);
        Strings.Assign ("New client ", LogMessage);
        Strings.Append (NameBuffer, LogMessage);
        LogTransaction (LogID, LogMessage);

        (* Check for too many clients. *)

        ClientNumber := UpdateCount (+1);
        IF ClientNumber = 0 THEN
            LogTransactionL (LogID, "Too many simultaneous clients");
            AbandonSession;
        END (*IF*);

        (* Register us with the watchdog.  *)

        NEW (KB);
        WITH KB^ DO
            SocketOpen := TRUE;  socket := S;
            TimedOut := FALSE;
        END (*WITH*);
        Obtain (MaxTimeLock);
        T := MaxTime;
        Release (MaxTimeLock);
        KB^.ID := AddToWatches (T, TimeoutHandler, KB);

        (* Open the session. *)

        NS := OpenNetStream (S, TRUE, LogID, useTLS);
        sess := OpenSession (NS, LogID, NameBuffer, KB^.ID);

        (* Here's the main command processing loop.  We leave it when   *)
        (* the client issues a QUIT command, or when socket             *)
        (* communications are lost, or when we get a watchdog timeout.  *)

        EndSession := FALSE;
        REPEAT
            KickWatchdog (KB^.ID);
            success := HandleOneRequest(sess, EndSession);
        UNTIL EndSession OR NOT success;
        CloseSession (sess);

        (* Work out whether the session was terminated deliberately,    *)
        (* or by a timeout or communications failure.                   *)

        IF (KB <> NIL) AND KB^.TimedOut THEN
            LogTransactionL (LogID, "Timed out");
        ELSIF EndSession THEN
            LogTransactionL (LogID, "Session terminated");
        ELSE
            LogTransactionL (LogID, "Communication failure");
        END (*IF*);

        CloseSession (sess);
        EVAL(soclose(S));
        S := NotASocket;
        KB^.SocketOpen := FALSE;
        AbandonSession;

    END SessionHandler;

(************************************************************************)
(*                       STARTING A NEW SESSION                         *)
(************************************************************************)

PROCEDURE NewSession (S: Socket;  addr: SockAddr;
                      LogID: TransactionLogID;  useTLS: BOOLEAN): BOOLEAN;

    (* Starts and runs a client session.  The session runs in a         *)
    (* separate thread; this procedure returns after starting the       *)
    (* session, it does not wait until the session is over.             *)

    VAR NSP: NewSessionPointer;  success: BOOLEAN;

    BEGIN
        NEW (NSP);
        WITH NSP^ DO
            socket := S;  IPAddress := addr.in_addr.addr;
            secure := useTLS;
        END (*WITH*);
        IF ResolveIP THEN
            StartNameLookup (NSP^.IPAddress);
        END (*IF*);
        success := CreateTask1 (SessionHandler, SessionPriority, "WebServe session", NSP);
        IF NOT success THEN
            IF ResolveIP THEN
                CancelNameLookup (NSP^.IPAddress);
            END (*IF*);
            CloseSocket (S, LogID);
            DISPOSE (NSP);
        END (*IF*);
        RETURN success;
    END NewSession;

(********************************************************************************)
(*                                  INITIALISATION                              *)
(********************************************************************************)

PROCEDURE SetSessionParameters (MaxSessions, seconds: CARDINAL;  lookuphosts: BOOLEAN);

    (* Sets the maximum number of sessions allowed in parallel, the time before *)
    (* an idle session times out, and whether we should look up client hostnames.*)

    BEGIN
        Obtain (ClientCountLock);
        MaxClients := MaxSessions;
        Release (ClientCountLock);
        Obtain (MaxTimeLock);
        IF seconds > MAX(CARDINAL) DIV 1000 THEN
            MaxTime := MAX(CARDINAL);
        ELSE
            MaxTime := 1000*seconds;
        END (*IF*);
        Release (MaxTimeLock);
        ResolveIP := lookuphosts;
    END SetSessionParameters;

(********************************************************************************)

BEGIN
    ResolveIP := FALSE;
    CreateLock (MaxTimeLock);
    CreateLock (ClientCountLock);
    Obtain (ClientCountLock);  ClientCount := 0;  Release (ClientCountLock);
    MaxClients := 10;
    LogToScreen := NotDetached();
    IF LogToScreen THEN
        UpdateTopScreenLine (69, "0 clients");
    END (*IF*);
END WSession.

