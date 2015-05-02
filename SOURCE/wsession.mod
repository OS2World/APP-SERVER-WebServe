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

IMPLEMENTATION MODULE WSession;

        (********************************************************)
        (*                                                      *)
        (*           Session handler for web server             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2015                    *)
        (*  Last edited:        29 April 2015                   *)
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

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release,
                CreateTask, CreateTask1, TaskExit;

FROM Timer IMPORT
    (* proc *)  Sleep, TimedWaitSpecial;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Signal;

FROM Queues IMPORT
    (* type *)  Queue,
    (* proc *)  CreateQueue, DestroyQueue, AddToQueue, TakeFromQueue;

FROM InetUtilities IMPORT
    (* proc *)  ConvertCardRJ, AppendCard, IPToString;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  StartTransactionLogging,
                CreateLogID, DiscardLogID, LogTransaction, LogTransactionL,
                UpdateTopScreenLine;

FROM Requests IMPORT
    (* type *)  Session,
    (* proc *)  LContext, OpenSession, CloseSession, HandleOneRequest;

(************************************************************************)

CONST
    SessionPriority = 3;
    NilLogID = CAST(TransactionLogID, NIL);
    NilSession = CAST(Session, NIL);
    NilSemaphore = CAST(Semaphore, NIL);

TYPE
    (* String form of a session identifier. *)

    SevenChar = ARRAY [0..7] OF CHAR;

    (* Data used in creating a new instance of the session handler task. *)

    NewSessionPointer = POINTER TO
                           RECORD
                               socket: Socket;
                               IPAddress: CARDINAL;
                           END (*RECORD*);

    (* Data needed by the timeout checker task. *)

    KeepAlivePointer = POINTER TO
                           RECORD
                               SocketOpen, dying: BOOLEAN;
                               WatchdogRunning: BOOLEAN;
                               TimedOut: BOOLEAN;
                               sem: Semaphore;
                               socket: Socket;
                               session: Session;
                               SessionID: SevenChar;
                           END (*RECORD*);

VAR
    (* A queue of KeepAlive records, which are passed to the    *)
    (* TimeoutChecker tasks as they are created.                *)

    KeepAliveQueue: Queue;

    (* Timeout delay, in milliseconds, and its mutual exclusion lock. *)

    MaxTime: CARDINAL;
    MaxTimeLock: Lock;

    (* Maximum allowed number of simultaneous clients.  For critical *)
    (* section protection we use lock ClientCountLock, see below.    *)

    MaxClients: CARDINAL;

    (* Count of active clients, and a lock to protect it. *)

    ClientCount: CARDINAL;
    ClientCountLock: Lock;

    (* Flag to control whether we write the number of clients to the screen. *)

    LogToScreen: BOOLEAN;

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
        IF value > MaxClients THEN
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
(*                         THE WATCHDOG TIMER                           *)
(************************************************************************)

PROCEDURE TimeoutChecker;

    (* A new instance of this task is created for each client session.  *)
    (* It kills the corresponding SessionHandler task if more than      *)
    (* MaxTime milliseconds have passed since the last Signal() on the  *)
    (* sessions KeepAlive semaphore.                                    *)

    (* This is a workaround.  I would have preferred to set the         *)
    (* timeout in the socket options, but I haven't yet figured out     *)
    (* how to do it.  An older version of IBM's sockets documentation   *)
    (* gave details on the send and receive timeouts, but this seems    *)
    (* to have disappeared from later versions of the documentation.    *)

    VAR p: KeepAlivePointer;  LogID: TransactionLogID;
        TimeLimit, KillCount: CARDINAL;

    BEGIN
        p := TakeFromQueue (KeepAliveQueue);
        p^.WatchdogRunning := TRUE;
        p^.SessionID[0] := 'W';
        LogID := CreateLogID (LContext(), p^.SessionID);
        LogTransactionL (LogID, "Watchdog timer started");
        REPEAT
            Obtain (MaxTimeLock);
            TimeLimit := MaxTime;
            Release (MaxTimeLock);
            TimedWaitSpecial (p^.sem, TimeLimit, p^.TimedOut);
        UNTIL p^.TimedOut OR p^.dying;
        IF p^.TimedOut THEN
            LogTransactionL (LogID, "Timeout, cancelling client socket");
            IF p^.socket <> NotASocket THEN
                so_cancel (p^.socket);
            END (*IF*);
            p^.socket := NotASocket;
        ELSE
            LogTransactionL (LogID, "Session terminated without timing out");
        END (*IF*);
        Sleep(50);     (* Let session close socket itself *)

        (* Wait for the socket to be closed. *)

        IF p^.SocketOpen THEN
            LogTransactionL (LogID, "Waiting for client socket to be closed");
        END (*IF*);
        KillCount := 0;
        WHILE p^.SocketOpen DO
            Sleep (5000);

            (* For the really stubborn cases, keep trying to cancel *)
            (* the sockets.                                         *)

            IF p^.SocketOpen THEN
                LogTransactionL (LogID, "Still trying to cancel client socket");
                so_cancel (p^.socket);
                p^.socket := NotASocket;
            END (*IF*);
            INC (KillCount);

            IF KillCount >= 10 THEN
                LogTransactionL (LogID, "Unable to cancel client socket, giving up");
                p^.SocketOpen := FALSE;
            END (*IF*);

        END (*WHILE*);

        LogTransactionL (LogID, "Watchdog timer closing");
        DiscardLogID (LogID);

        (* Note that the KeepAlive record still exists.  We leave it    *)
        (* up to the client thread to discard it.                       *)

        p^.WatchdogRunning := FALSE;

    END TimeoutChecker;

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
        KeepAliveSemaphore: Semaphore;
        KA: KeepAlivePointer;

    (********************************************************************)

    PROCEDURE AbandonSession;

        (* Release resources and exit on premature session close.  It's *)
        (* less error-prone to handle the resource release at a single  *)
        (* point.                                                       *)

        BEGIN
            IF sess <> NilSession THEN
                CloseSession (sess);
            END (*IF*);
            IF KeepAliveSemaphore <> NilSemaphore THEN
                DestroySemaphore (KeepAliveSemaphore);
            END (*IF*);
            IF KA <> NIL THEN
                DISPOSE (KA);
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
        IPAddress: CARDINAL;
        IPBuffer: ARRAY [0..16] OF CHAR;
        SessionNumber: SevenChar;
        LogMessage: ARRAY [0..127] OF CHAR;
        EndSession, success: BOOLEAN;

    BEGIN
        DosError (FERR_DISABLEHARDERR);
        NSP := arg;
        S := NSP^.socket;
        IPAddress := NSP^.IPAddress;
        DISPOSE (NSP);

        sess := NIL;
        KeepAliveSemaphore := NilSemaphore;
        KA := NIL;

        (* Create the log file ID for this session. *)

        CardinalToString (S, SessionNumber, 7);  SessionNumber[7] := CHR(0);
        LogID := CreateLogID (LContext(), SessionNumber);

        (* Initial transaction log message. *)

        IPToString (IPAddress, TRUE, IPBuffer);
        Strings.Assign ("New client ", LogMessage);
        Strings.Append (IPBuffer, LogMessage);
        LogTransaction (LogID, LogMessage);

        (* Check for too many clients. *)

        ClientNumber := UpdateCount (+1);
        IF ClientNumber = 0 THEN
            LogTransactionL (LogID, "Too many simultaneous clients");
            AbandonSession;
        END (*IF*);

        CreateSemaphore (KeepAliveSemaphore, 0);
        sess := OpenSession (S, LogID);

        (* Create an instance of the TimeoutChecker task. *)

        NEW (KA);
        WITH KA^ DO
            SocketOpen := TRUE;  socket := S;
            WatchdogRunning := TRUE;
            dying := FALSE;
            sem := KeepAliveSemaphore;
            session := sess;
            SessionID := SessionNumber;
            TimedOut := FALSE;
        END (*WITH*);

        IF CreateTask (TimeoutChecker, SessionPriority+2, "WebServe timeout") THEN
            AddToQueue (KeepAliveQueue, KA);
        ELSE
            LogTransactionL (LogID, "Failed to create watchdog thread");
            AbandonSession;
        END (*IF*);

        (* Here's the main command processing loop.  We leave it when   *)
        (* the client issues a QUIT command, or when socket             *)
        (* communications are lost, or when we get a timeout on the     *)
        (* KeepAlive semaphore.                                         *)

        EndSession := FALSE;
        REPEAT
            Signal (KA^.sem);
            success := HandleOneRequest(sess, EndSession);
        UNTIL EndSession OR NOT success;
        CloseSession (sess);

        (* Work out whether the session was terminated deliberately,    *)
        (* or by a timeout or communications failure.                   *)

        IF KA^.TimedOut THEN
            LogTransactionL (LogID, "Timed out");
        ELSIF EndSession THEN
            LogTransactionL (LogID, "Session terminated");
        ELSE
            LogTransactionL (LogID, "Communication failure");
        END (*IF*);

        (* Note potential race condition.  When we close the session,   *)
        (* sess is NIL but the watchdog thread also has a copy of this  *)
        (* pointer.  It's important to clear the copy so that the       *)
        (* watchdog does not try to use an obsolete pointer.            *)

        KA^.session := NIL;
        CloseSession (sess);
        KA^.dying := TRUE;  Signal (KA^.sem);
        EVAL(soclose(S));
        KA^.SocketOpen := FALSE;
        WHILE KA^.WatchdogRunning DO
            Signal (KA^.sem);
            Sleep (500);
        END (*WHILE*);
        AbandonSession;

    END SessionHandler;

(************************************************************************)
(*                       STARTING A NEW SESSION                         *)
(************************************************************************)

PROCEDURE NewSession (S: Socket;  addr: SockAddr;
                         LogID: TransactionLogID): BOOLEAN;

    (* Starts and runs a client session.  The session runs in a         *)
    (* separate thread; this procedure returns after starting the       *)
    (* session, it does not wait until the session is over.             *)

    VAR NSP: NewSessionPointer;  success: BOOLEAN;

    BEGIN
        NEW (NSP);
        WITH NSP^ DO
            socket := S;  IPAddress := addr.in_addr.addr;
        END (*WITH*);
        success := CreateTask1 (SessionHandler, SessionPriority, "WebServe session", NSP);
        IF NOT success THEN
            CloseSocket (S, LogID);
            DISPOSE (NSP);
        END (*IF*);
        RETURN success;
    END NewSession;

(********************************************************************************)
(*                                  INITIALISATION                              *)
(********************************************************************************)

PROCEDURE SetSessionParameters (MaxSessions, seconds: CARDINAL);

    (* Sets the maximum number of sessions allowed in parallel, and the time    *)
    (* before an idle session times out.                                        *)

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
    END SetSessionParameters;

(********************************************************************************)

BEGIN
    CreateLock (MaxTimeLock);
    CreateLock (ClientCountLock);
    Obtain (ClientCountLock);  ClientCount := 0;  Release (ClientCountLock);
    CreateQueue (KeepAliveQueue);
    MaxClients := 10;
    LogToScreen := NotDetached();
    IF LogToScreen THEN
        UpdateTopScreenLine (69, "0 clients");
    END (*IF*);
FINALLY
    DestroyLock (MaxTimeLock);
    DestroyLock (ClientCountLock);

    (* Don't destroy KeepAliveQueue because timeout checker might still be running. *)

END WSession.

