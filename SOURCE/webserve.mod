(**************************************************************************)
(*                                                                        *)
(*  The WebServe web server                                               *)
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

MODULE WebServe;

        (********************************************************)
        (*                                                      *)
        (*              A lightweight web server                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2015                    *)
        (*  Last edited:        1 August 2016                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*     The relevant standard is RFC2616                 *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(*  WHAT WE CAN AND CAN'T HANDLE                                        *)
(*                                                                      *)
(*      Cannot operate as a proxy.                                      *)
(*      Have not checked whether it works with HTTP/1.0 client.         *)
(*      Only requests handled are GET and HEAD.                         *)
(*      See comments at beginning of module Requests for some things    *)
(*          not yet implemented.                                        *)
(*                                                                      *)
(************************************************************************)

IMPORT WSV, SYSTEM, OS2, TextIO, Strings;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, INIValid, CloseINIFile, INIGet;

FROM Internet IMPORT
    (* const*)  Zero8, INADDR_ANY;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, SockAddr, AddressFamily, SocketType,
    (* proc *)  sock_init, socket, so_cancel, setsockopt,
                bind, listen, select, accept, soclose, psock_errno,
                getsockname, getpeername, sock_errno;

FROM ProgName IMPORT
    (* proc *)  GetProgramName;

FROM Exceptq IMPORT
    (* proc *)  InstallExceptq, UninstallExceptq;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID,
                LogTransaction, LogTransactionL,
                UpdateTopScreenLine;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM CtrlC IMPORT
    (* proc *)  SetBreakHandler;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM InetUtilities IMPORT
    (* proc *)  AppendCard, Swap2, WaitForSocket;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached;

FROM MyClock IMPORT
    (* proc *)  OurTimezone;

FROM WSession IMPORT
    (* proc *)  SetSessionParameters, NewSession, NumberOfClients;

FROM Requests IMPORT
    (* proc *)  LContext, LoadReqINIData;

(************************************************************************)

CONST
    Nul = CHR(0);

VAR
    (* Program name and version. *)

    ProgVersion: ARRAY [0..31] OF CHAR;

    (* Our INI file name. *)

    ININame: ARRAY [0..31] OF CHAR;
    UseTNI: BOOLEAN;

    (* Event semaphore to trigger updater task. *)

    UpdaterFlag: OS2.HEV;

    (* Flags used in shutdown processing. *)

    ShutdownInProgress, RapidShutdown: BOOLEAN;
    ShutdownRequest, TaskDone: Semaphore;

    (* Flag to say that we're not running detached. *)

    ScreenEnabled: BOOLEAN;

    (* The socket on which we listen for connection attempts,   *)
    (* and the server port on which we listen.                  *)

    MainSocket: Socket;
    ServerPort: CARDINAL;

(************************************************************************)
(*                           SHUTTING DOWN                              *)
(************************************************************************)

PROCEDURE ShutdownChecker;

    (* A separate task that waits for a shutdown request.  *)

    VAR StillRunning: BOOLEAN;

    BEGIN
        StillRunning := TRUE;
        WHILE NOT RapidShutdown DO
            Wait (ShutdownRequest);
            RapidShutdown := ShutdownInProgress;
            ShutdownInProgress := TRUE;
            IF StillRunning THEN
                IF MainSocket <> NotASocket THEN
                    so_cancel (MainSocket);
                END (*IF*);
                StillRunning := FALSE;
            END (*IF*);
        END (*WHILE*);
        Signal (TaskDone);
    END ShutdownChecker;

(************************************************************************)

PROCEDURE ["C"] ControlCHandler(): BOOLEAN;

    (* Intercepts a Ctrl/C from the keyboard. *)

    BEGIN
        Signal (ShutdownRequest);
        RETURN TRUE;
    END ControlCHandler;

(************************************************************************)
(*                           LOADING THE INI DATA                       *)
(************************************************************************)

PROCEDURE LoadUpdateableINIData;

    VAR hini: HINI;
        MaxClients, timeout: CARDINAL;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF SYSTEM.LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (********************************************************************)

    BEGIN
        SYSapp := "$SYS";
        MaxClients := 100;
        timeout := 120;
        hini := OpenINIFile(ININame, UseTNI);
        IF INIValid (hini) THEN
            EVAL (GetItem ("MaxClients", MaxClients));
            EVAL (GetItem ("Timeout", timeout));
            CloseINIFile (hini);
        END (*IF*);
        SetSessionParameters (MaxClients, timeout);
        LoadReqINIData (ININame, UseTNI);
    END LoadUpdateableINIData;

(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "webserve.ini" or "webserve.tni". *)

    VAR hini: HINI;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF SYSTEM.LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (********************************************************************)

    BEGIN
        SYSapp := "$SYS";
        IF UseTNI THEN
            ININame := "WebServe.tni";
            hini := OpenINIFile(ININame, TRUE);
        ELSE
            ININame := "WebServe.ini";
            hini := OpenINIFile(ININame, FALSE);
        END (*IF*);
        IF INIValid (hini) THEN
            IF NOT GetItem ("ServerPort", ServerPort) THEN
                ServerPort := 80;
            END (*IF*);
            CloseINIFile (hini);
        END (*IF*);

        LoadUpdateableINIData;

    END LoadINIData;

(********************************************************************************)
(*                    TASK TO CATCH UPDATES TO THE INI DATA                     *)
(********************************************************************************)

PROCEDURE INIChangeDetector;

    (* Runs as a separate task.  Rereads some of the configuration data each    *)
    (* time a public event semaphore tells us that there's been a change.       *)

    VAR count: CARDINAL;
        semName: ARRAY [0..127] OF CHAR;
        LogID: TransactionLogID;

    BEGIN
        semName := "\SEM32\";
        Strings.Append ("WEBSERVE", semName);
        Strings.Append ("\UPDATED", semName);
        LogID := CreateLogID (LContext(), "INImon ");
        UpdaterFlag := 0;
        IF OS2.DosOpenEventSem (semName, UpdaterFlag) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, UpdaterFlag, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        WHILE NOT ShutdownInProgress DO
            OS2.DosWaitEventSem (UpdaterFlag, OS2.SEM_INDEFINITE_WAIT);
            OS2.DosResetEventSem (UpdaterFlag, count);
            IF NOT ShutdownInProgress THEN
                LogTransactionL (LogID, "Reloading the INI data");
                LoadUpdateableINIData;
            END (*IF*);
        END (*WHILE*);

        OS2.DosCloseEventSem(UpdaterFlag);
        Signal (TaskDone);

    END INIChangeDetector;

(************************************************************************)
(*                         COMMAND LINE ARGUMENTS                       *)
(************************************************************************)

PROCEDURE GetParameters;

    (* Picks up optional program arguments from the command line. *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    VAR j: CARDINAL;  ch: CHAR;
        args: ChanId;
        ParameterString: ARRAY [0..79] OF CHAR;

    BEGIN
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, ParameterString);
            j := 0;
            LOOP
                ch := ParameterString[j];  INC(j);
                IF ch = Nul THEN
                    EXIT (*LOOP*);
                ELSIF ch = '-' THEN
                    (* Ignored in this version *);
                ELSIF CAP(ch) = 'I' THEN
                    UseTNI := FALSE;
                ELSIF CAP(ch) = 'T' THEN
                    UseTNI := TRUE;
                ELSIF ch <> ' ' THEN
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
        END (*IF*);

    END GetParameters;

(************************************************************************)
(*                           THE MAIN SERVER CODE                       *)
(************************************************************************)

PROCEDURE WriteError (LogID: TransactionLogID);

    VAR LogLine: ARRAY [0..255] OF CHAR;

    BEGIN
        Strings.Assign ("Socket error ", LogLine);
        AppendCard (sock_errno(), LogLine);
        LogTransaction (LogID, LogLine);
    END WriteError;

(************************************************************************)

PROCEDURE RunTheServer;

    (*  OPERATING AS A SERVER                                                       *)
    (*     1. (Compulsory) Call "bind" to bind the socket with a local address.     *)
    (*        You can usually afford to specify INADDR_ANY as the machine           *)
    (*        address, but you'd normally bind to a specific port number.           *)
    (*     2. Call "listen" to indicate your willingness to accept connections.     *)
    (*     3. Call "accept", getting a new socket (say ns) from the client.         *)
    (*     4. Use procedures "send" and "recv" to transfer data, using socket ns.   *)
    (*        (Meanwhile, your original socket remains available to accept          *)
    (*        more connections, so you can continue with more "accept" operations   *)
    (*        in parallel with these data operations.  If so, you should of course  *)
    (*        be prepared to run multiple threads.)                                 *)
    (*     5. Use "soclose(ns)" to terminate the session with that particular       *)
    (*        client.                                                               *)
    (*     6. Use "soclose" on your original socket to clean up at the end.         *)

    VAR
        LogID: TransactionLogID;
        temp: CARDINAL;
        ns: Socket;  myaddr, client: SockAddr;
        ExceptqActive: BOOLEAN;
        StartupSuccessful: BOOLEAN;
        exRegRec: OS2.EXCEPTIONREGISTRATIONRECORD;
        message: ARRAY [0..127] OF CHAR;
        tzstring: ARRAY [0..7] OF CHAR;

    BEGIN
        (* Initial setup and opening log messages. *)

        LogID := CreateLogID (LContext(), "       ");
        IF sock_init() <> 0 THEN
            LogTransactionL (LogID, "No network.");
            DiscardLogID (LogID);
            RETURN;
        END (*IF*);

        ExceptqActive := InstallExceptq (exRegRec);
        IF NOT ExceptqActive THEN
            LogTransactionL (LogID, "Failed to load the Exceptq handler.");
        END (*IF*);

        IF UseTNI THEN
            LogTransactionL (LogID, "Getting configuration data from webserve.tni");
        ELSE
            LogTransactionL (LogID, "Getting configuration data from webserve.ini");
        END (*IF*);

        IF UseTNI THEN
            message := "[T] ";
        ELSE
            message := "[I] ";
        END (*IF*);
        Strings.Append (ProgVersion, message);
        LogTransaction (LogID, message);

        IF ScreenEnabled THEN
            Strings.Append ("            ", message);
            UpdateTopScreenLine (0, message);
            UpdateTopScreenLine (25, "(C) 2016 Peter Moylan");

            EVAL (SetBreakHandler (ControlCHandler));
        END (*IF*);

        message := "exceptq support is ";
        IF ExceptqActive THEN
            Strings.Append ("present", message);
        ELSE
            Strings.Append ("absent", message);
        END (*IF*);
        LogTransaction (LogID, message);

        message := "Time zone is ";
        OurTimezone (tzstring);
        IF tzstring[0] = Nul THEN
            Strings.Append ("undefined, please run TZSet", message);
        ELSE
            Strings.Append (tzstring, message);
        END (*IF*);
        LogTransaction (LogID, message);

        (* Now start up the server. *)

        StartupSuccessful := FALSE;

        MainSocket := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);

            (* Allow reuse of the port we're binding to. *)

        temp := 1;
        setsockopt (MainSocket, 0FFFFH, 4, temp, SIZE(CARDINAL));

        Strings.Assign ("Listening on port ", message);
        AppendCard (ServerPort, message);
        Strings.Append (", socket ", message);
        AppendCard (MainSocket, message);
        LogTransaction (LogID, message);

        (* Now have the socket, bind to our machine. *)

        WITH myaddr DO
            family := AF_INET;
            WITH in_addr DO
                port := Swap2 (ServerPort);
                (* Bind to all interfaces. *)
                addr := INADDR_ANY;
                zero := Zero8;
            END (*WITH*);
        END (*WITH*);

        IF bind (MainSocket, myaddr, SIZE(myaddr)) THEN

            WriteError (LogID);
            LogTransactionL (LogID, "Cannot bind to server port.");

        ELSE

            (* Go into listening mode. *)

            IF listen (MainSocket, 5) THEN
                WriteError (LogID);
            ELSE
                StartupSuccessful := TRUE;
            END (*IF*);

        END (*IF bind*);

        IF StartupSuccessful THEN

            Strings.Assign (ProgVersion, message);
            Strings.Append (" started.", message);
            LogTransaction (LogID, message);

            (* Here's the main service loop. *)

            WHILE WaitForSocket (MainSocket, MAX(CARDINAL)) > 0 DO
                temp := SIZE(client);
                ns := accept (MainSocket, client, temp);
                IF ns <> NotASocket THEN
                    IF NOT NewSession (ns, client, LogID) THEN
                        LogTransactionL (LogID,
                            "Failed to create session");
                    END (*IF*);
                END (*IF*);
            END (*WHILE*);

            (* Close the main socket. *)

            IF  soclose(MainSocket) THEN
                psock_errno ("");
            END (*IF*);

        END (*IF*);

        (* End of operation, shut down the server. *)

        Signal (ShutdownRequest);
        IF NOT RapidShutdown THEN
            IF NumberOfClients() > 0 THEN
                LogTransactionL (LogID, "Waiting for existing clients to finish");
            END (*IF*);
            WHILE (NumberOfClients() > 0) AND NOT RapidShutdown DO
                Sleep (1000);
            END (*WHILE*);
        END (*IF*);
        RapidShutdown := TRUE;

        LogTransactionL (LogID, "WebServe closing down");
        DiscardLogID (LogID);

        UninstallExceptq (exRegRec);
        Signal (TaskDone);

    END RunTheServer;

(********************************************************************************)
(*                                PROGRAM START                                 *)
(********************************************************************************)

BEGIN
    GetProgramName (ProgVersion);
    ScreenEnabled := NotDetached();
    UseTNI := FALSE;
    ServerPort := 80;
    ShutdownInProgress := FALSE;  RapidShutdown := FALSE;
    CreateSemaphore (TaskDone, 0);
    CreateSemaphore (ShutdownRequest, 0);
    EVAL(CreateTask (ShutdownChecker, 1, "ctrl/c hook"));
    EVAL(CreateTask (INIChangeDetector, 2, "update"));
    GetParameters;
    LoadINIData;
    RunTheServer;
FINALLY
    OS2.DosPostEventSem (UpdaterFlag);
    Wait (TaskDone);
    Wait (TaskDone);
    Wait (TaskDone);
END WebServe.

