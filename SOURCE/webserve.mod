(**************************************************************************)
(*                                                                        *)
(*  The WebServe web server                                               *)
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

MODULE WebServe;

        (********************************************************)
        (*                                                      *)
        (*              A lightweight web server                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 March 2015                    *)
        (*  Last edited:        13 November 2018                *)
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
(*      Only requests handled are GET, HEAD, and POST.                  *)
(*      See comments at beginning of module Requests for some things    *)
(*          not yet implemented.                                        *)
(*      HTTPS not yet working.                                          *)
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
                getsockname, sock_errno;

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

FROM MiscFuncs IMPORT
    (* proc *)  AppendCard;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, WaitForSocket;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached;

FROM MyClock IMPORT
    (* proc *)  OurTimezone;

FROM WSession IMPORT
    (* proc *)  SetSessionParameters, NewSession, NumberOfClients;

FROM Requests IMPORT
    (* proc *)  LContext, LoadReqINIData;

(************************************************************************)

TYPE
    ServiceType = (HTTP, HTTPS);
    SocketArray = ARRAY ServiceType OF Socket;
    ServiceNameArray = ARRAY ServiceType OF ARRAY [0..4] OF CHAR;
    CardArray = ARRAY ServiceType OF CARDINAL;

CONST
    Nul = CHR(0);
    ServiceName = ServiceNameArray {"HTTP", "HTTPS"};

VAR
    (* Program name and version. *)

    ProgVersion: ARRAY [0..31] OF CHAR;

    (* Our INI file name. *)

    ININame: ARRAY [0..31] OF CHAR;
    UseTNI: BOOLEAN;

    (* Event semaphore to trigger updater task. *)

    UpdaterFlag: OS2.HEV;

    (* Services enabled. *)

    ServerEnabled: SYSTEM.CARD8;

    (* Port for each service type. *)

    ServerPort: CardArray;

    (* Socket value for each service type. *)

    MainSocket: SocketArray;

    (* Event semaphore by which an external program requests a shutdown. *)

    ExternalShutdownRequest: OS2.HEV;

    (* Flags used in shutdown processing. *)

    ShutdownInProgress, RapidShutdown: BOOLEAN;
    ShutdownRequest, TaskDone: Semaphore;

    (* Flag to say that we're not running detached. *)

    ScreenEnabled: BOOLEAN;

(************************************************************************)
(*                           SHUTTING DOWN                              *)
(************************************************************************)

PROCEDURE ShutdownChecker;

    (* A separate task that waits for a shutdown request.  *)

    VAR StillRunning: BOOLEAN;  j: ServiceType;

    BEGIN
        StillRunning := TRUE;
        WHILE NOT RapidShutdown DO
            Wait (ShutdownRequest);
            RapidShutdown := ShutdownInProgress;
            ShutdownInProgress := TRUE;
            IF StillRunning THEN
                FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
                    IF MainSocket[j] <> NotASocket THEN
                        so_cancel (MainSocket[j]);
                    END (*IF*);
                END (*FOR*);
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

(********************************************************************************)
(*                   TASK TO CATCH EXTERNAL SHUTDOWN REQUESTS                   *)
(********************************************************************************)

PROCEDURE ShutdownRequestDetector;

    (* Runs as a separate task.  Detects a signal on the global event semaphore *)
    (* by which an external program can request a shutdown.                     *)

    CONST semName = "\SEM32\WEBSERVE\SHUTDOWN";

    VAR count: CARDINAL;

    BEGIN
        ExternalShutdownRequest := 0;
        IF OS2.DosOpenEventSem (semName, ExternalShutdownRequest) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, ExternalShutdownRequest, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        (* We treat a signal on this event semaphore in the same way as we      *)
        (* treat a CTRL/C.  In particular, we accept it more than once.         *)

        WHILE NOT RapidShutdown DO
            OS2.DosWaitEventSem (ExternalShutdownRequest, OS2.SEM_INDEFINITE_WAIT);
            OS2.DosResetEventSem (ExternalShutdownRequest, count);
            Signal (ShutdownRequest);
            IF count > 0 THEN
                Signal (ShutdownRequest);
            END (*IF*);
        END (*WHILE*);

        OS2.DosCloseEventSem(ExternalShutdownRequest);

    END ShutdownRequestDetector;

(********************************************************************************)
(*           PROCEDURE TO TELL THE OUTSIDE WORLD THAT WE'VE FINISHED            *)
(********************************************************************************)

PROCEDURE NotifyTermination;

    (* Posts the global event semaphore that tells other programs that Weasel   *)
    (* has shut down.                                                           *)

    CONST semName = "\SEM32\WEBSERVE\FINISHED";

    VAR hev: OS2.HEV;

    BEGIN
        hev := 0;
        IF OS2.DosOpenEventSem (semName, hev) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, hev, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);
        OS2.DosPostEventSem (hev);
        OS2.DosCloseEventSem(hev);
    END NotifyTermination;

(************************************************************************)
(*                           LOADING THE INI DATA                       *)
(************************************************************************)

PROCEDURE LoadUpdateableINIData;

    VAR hini: HINI;
        MaxClients, timeout: CARDINAL;
        SYSapp: ARRAY [0..4] OF CHAR;
        ResolveIP: BOOLEAN;

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
        ResolveIP := FALSE;
        hini := OpenINIFile(ININame, UseTNI);
        IF INIValid (hini) THEN
            EVAL (GetItem ("MaxClients", MaxClients));
            EVAL (GetItem ("Timeout", timeout));
            EVAL (GetItem ("ResolveIP", ResolveIP));
            CloseINIFile (hini);
        END (*IF*);
        SetSessionParameters (MaxClients, timeout, ResolveIP);
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
            IF NOT GetItem ("Enable", ServerEnabled) THEN
                ServerEnabled := 1;
            END (*IF*);
            IF NOT GetItem ("ServerPort2", ServerPort) THEN
                IF NOT GetItem ("ServerPort", ServerPort[HTTP]) THEN
                    ServerPort[HTTP] := 80;
                END (*IF*);
                ServerPort[HTTPS] := 443;
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

    TYPE
        TestType = [0..1];
        TestArray = ARRAY TestType OF Socket;

    VAR
        LogID: TransactionLogID;
        temp: CARDINAL;
        SocketsToTest, DefaultSocketsToTest: TestArray;
        ServiceToTestMap: ARRAY ServiceType OF TestType;
        j: ServiceType;
        k, nservice: TestType;
        Enabled: ARRAY ServiceType OF BOOLEAN;
        ns: Socket;  myaddr, client: SockAddr;
        ExceptqActive: BOOLEAN;
        StartupSuccessful: BOOLEAN;
        optval: CARDINAL;
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
            UpdateTopScreenLine (25, "(C) 2018 Peter Moylan");

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

        Enabled[HTTP] := ODD(ServerEnabled);
        Enabled[HTTPS] := ODD(ServerEnabled DIV 2);
        StartupSuccessful := FALSE;

        FOR k := 0 TO MAX(TestType) DO
            DefaultSocketsToTest[k] := NotASocket;
        END (*FOR*);
        nservice := 0;

        FOR j := MIN(ServiceType) TO MAX(ServiceType) DO

            MainSocket[j] := NotASocket;

            IF Enabled[j] THEN
                MainSocket[j] := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
                ServiceToTestMap[j] := nservice;
                DefaultSocketsToTest[nservice] := MainSocket[j];
                INC (nservice);

                (* Allow reuse of the port we're binding to. *)

                optval := 1;
                setsockopt (MainSocket[j], 0FFFFH, 4, optval, SIZE(optval));
            ELSE
                ServiceToTestMap[j] := MAX(TestType);
            END (*IF*);

            Strings.Assign (ServiceName[j], message);
            IF Enabled[j] THEN
                Strings.Append (" listening on ", message);
                Strings.Append ("all interfaces", message);
                Strings.Append (", port ", message);
                AppendCard (ServerPort[j], message);
            ELSE
                Strings.Append (" disabled.", message);
            END (*IF*);
            LogTransaction (LogID, message);

            IF Enabled[j] THEN

                (* Now have the socket, bind to our machine. *)

                (* In the present version we bind to all interfaces for     *)
                (* incoming connections, and only use BindAddr for          *)
                (* outgoing mail -- see modules Domains and Delivery.       *)

                WITH myaddr DO
                    family := AF_INET;
                    WITH in_addr DO
                        port := Swap2 (ServerPort[j]);
                        (*addr := BindAddr;*)
                        addr := INADDR_ANY;
                        zero := Zero8;
                    END (*WITH*);
                END (*WITH*);

                IF bind (MainSocket[j], myaddr, SIZE(myaddr)) THEN

                    WriteError (LogID);
                    LogTransactionL (LogID, "Cannot bind to server port.");

                ELSE

                    (* Go into listening mode. *)

                    IF listen (MainSocket[j], 5) THEN
                        WriteError (LogID);
                    ELSE
                        StartupSuccessful := TRUE;
                    END (*IF*);

                END (*IF bind*);

            END (*IF Enabled*);

        END (*FOR*);

        IF StartupSuccessful THEN

            Strings.Assign (ProgVersion, message);
            Strings.Append (" started.", message);
            LogTransaction (LogID, message);

            (* Here's the main service loop. *)

            (*
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
            *)

            (* Here's the main service loop. *)

            SocketsToTest := DefaultSocketsToTest;
            WHILE select (SocketsToTest, nservice, 0, 0, MAX(CARDINAL)) > 0 DO
                FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
                    IF Enabled[j] THEN
                        k := ServiceToTestMap[j];
                        IF SocketsToTest[k] <> NotASocket THEN
                            temp := SIZE(client);
                            ns := accept (MainSocket[j], client, temp);
                            IF ns <> NotASocket THEN
                                IF NOT NewSession (ns, client, LogID, j=HTTPS) THEN
                                    LogTransactionL (LogID,
                                        "Failed to create session");
                                END (*IF*);
                            END (*IF*);
                        END (*IF*);
                    END (*IF*);
                END (*FOR*);
                SocketsToTest := DefaultSocketsToTest;
            END (*WHILE*);

            (* Close all open main sockets. *)

            FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
                IF (MainSocket[j] <> NotASocket) AND soclose(MainSocket[j]) THEN
                    psock_errno ("");
                END (*IF*);
            END (*FOR*);

            (* Close the main socket. *)

            (*
            IF  soclose(MainSocket) THEN
                psock_errno ("");
            END (*IF*);
            *)

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
    ServerEnabled := 1;
    UseTNI := FALSE;
    ServerPort[HTTP] := 80;
    ShutdownInProgress := FALSE;  RapidShutdown := FALSE;
    CreateSemaphore (TaskDone, 0);
    CreateSemaphore (ShutdownRequest, 0);
    EVAL(CreateTask (ShutdownChecker, 1, "ctrl/c hook"));
    EVAL(CreateTask (INIChangeDetector, 2, "update"));
    EVAL(CreateTask (ShutdownRequestDetector, 2, "shutdown"));
    GetParameters;
    LoadINIData;
    RunTheServer;
FINALLY
    OS2.DosPostEventSem (UpdaterFlag);
    Wait (TaskDone);
    Wait (TaskDone);
    NotifyTermination;
END WebServe.

