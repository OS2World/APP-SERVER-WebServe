(**************************************************************************)
(*                                                                        *)
(*  FtpServer FTP daemon                                                  *)
(*  Copyright (C) 2014   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE NameLookup;

        (********************************************************)
        (*                                                      *)
        (*                   Host name lookup                   *)
        (*                                                      *)
        (*   We put this service into a separate module so that *)
        (*   we can arrange to contact the nameserver in a      *)
        (*   separate thread.  This means that there will be    *)
        (*   no delays if the nameserver is down or slow.       *)
        (*   If we don't have an answer by the time it's        *)
        (*   needed, we can get by with a null name.            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            23 March 1998                   *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Sockets IMPORT
    (* type *)  AddressFamily;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM NetDB IMPORT
    (* type *)  HostEntPtr,
    (* proc *)  gethostbyaddr;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release, CreateTask;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

(************************************************************************)

TYPE
    NameString = ARRAY [0..511] OF CHAR;

    (* The 'count' field allows for the possibility that there can be   *)
    (* several outstanding requests for one particular IP address.      *)
    (* This can happen legitimately in some cases, but more importantly *)
    (* it can happen in a denial-of-service attack where we are being   *)
    (* hit repeatedly from the same address, and in such cases we can   *)
    (* save ourselves a lot of overhead and confusion by combining the  *)
    (* requests.                                                        *)

    ElementPointer = POINTER TO
                         RECORD
                             next: ElementPointer;
                             count: CARDINAL;
                             Address: CARDINAL;
                             HostName: NameString;
                         END (*RECORD*);

(************************************************************************)

CONST
    Nul = CHR(0);

VAR
    (* Queue of translation requests.  New requests are added at the    *)
    (* tail of the queue.  (An exception is where the new request is a  *)
    (* duplicate of an existing request, in which case we increment the *)
    (* corresponding count rather than adding a new queue element.)     *)
    (* The "nexttoconvert" field points at the first entry which is     *)
    (* not yet translated.  Results may be pulled out of the queue in   *)
    (* any order.  In particular it's possible to pull out a            *)
    (* not-yet-computed result.  In that case we decide that the        *)
    (* nameserver has not been fast enough for us, and we return an     *)
    (* empty string as the result.                                      *)

    Requests: RECORD
                  head, nexttoconvert, tail: ElementPointer;
                  access: Lock;
                  ConversionRequest: Semaphore;
              END (*RECORD*);

(************************************************************************)
(*                       QUEUE MANIPULATIONS                            *)
(************************************************************************)

PROCEDURE TakeFromQueue (key: CARDINAL;  Retain: BOOLEAN;
                                            VAR (*OUT*) Name: NameString);

    (* Searches the queue for an address field match, removes the       *)
    (* corresponding request from the queue and returns the Name.  A    *)
    (* null Name means that no match was found.  If Retain = TRUE then  *)
    (* we return a result but we still leave the request in the queue.  *)

    VAR previous, result: ElementPointer;

    BEGIN
        Obtain (Requests.access);

        previous := NIL;  result := Requests.head;
        WHILE (result <> NIL) AND (result^.Address <> key) DO
            previous := result;
            result := result^.next;
        END (*WHILE*);

        IF result = NIL THEN
            Name[0] := Nul;
        ELSE
            Strings.Assign (result^.HostName, Name);
            IF NOT Retain THEN
                DEC (result^.count);
            END (*IF*);
            IF result^.count = 0 THEN

                (* We no longer need this record in the queue, so *)
                (* update the queue pointers.                     *)

                IF result = Requests.nexttoconvert THEN
                    Requests.nexttoconvert := result^.next;
                END (*IF*);
                IF result = Requests.tail THEN
                    Requests.tail := previous;
                END (*IF*);
                IF previous = NIL THEN
                    Requests.head := result^.next;
                ELSE
                    previous^.next := result^.next;
                END (*IF*);
                DISPOSE (result);

            END (*IF*);
        END (*IF*);

        Release (Requests.access);

    END TakeFromQueue;

(************************************************************************)
(*                         NAMESERVER ENQUIRIES                         *)
(************************************************************************)

PROCEDURE TranslationTask;

    (* Runs as an autonomous task, working through the request queue    *)
    (* and translating each entry as it comes to it.                    *)

    VAR NameDetails: HostEntPtr;
        addr: CARDINAL;

    BEGIN
        LOOP
            WITH Requests DO

                (* Get the next translation request. *)

                Wait (ConversionRequest);
                Obtain (Requests.access);

                (* Watch out for cancelled requests. *)

                IF nexttoconvert <> NIL THEN
                    addr := nexttoconvert^.Address;
                    Release (Requests.access);

                    (* Perform a nameserver lookup.  (This can be slow, *)
                    (* which is why we release the access lock while    *)
                    (* doing the lookup.)                               *)

                    NameDetails := gethostbyaddr (addr, SIZE(CARDINAL), AF_INET);

                    (* Put the result back into the queue.  Beware of     *)
                    (* the situation where the request has been cancelled.*)

                    Obtain (Requests.access);
                    IF (nexttoconvert <> NIL) AND (nexttoconvert^.Address = addr) THEN
                        IF (NameDetails <> NIL) AND (NameDetails^.h_name <> NIL) THEN
                            Strings.Assign (NameDetails^.h_name^, nexttoconvert^.HostName);
                        ELSE
                            nexttoconvert^.HostName[0] := Nul;
                        END (*IF*);
                        nexttoconvert := nexttoconvert^.next;
                    END (*IF*);
                END (*IF*);

                Release (Requests.access);

            END (*WITH*);

        END (*LOOP*);

    END TranslationTask;

(************************************************************************)
(*                       THE CLIENT INTERFACE                           *)
(************************************************************************)

PROCEDURE StartNameLookup (IPAddress: CARDINAL);

    (* Enters a request for translating an IP address to a node name.   *)

    VAR current, p: ElementPointer;

    BEGIN
        Obtain (Requests.access);

        (* Start searching from the beginning of the list, in case the  *)
        (* IP address we want is already on the list.                   *)

        current := Requests.head;
        WHILE (current <> NIL) AND (current^.Address <> IPAddress) DO
            current := current^.next;
        END (*WHILE*);

        IF current = NIL THEN

            (* Create a new entry at the tail of the list. *)

            NEW (p);
            WITH p^ DO
                next := NIL;
                count := 1;
                Address := IPAddress;
                HostName[0] := Nul;
            END (*WITH*);

            WITH Requests DO
                IF tail = NIL THEN
                    head := p;
                ELSE
                    tail^.next := p;
                END (*IF*);
                tail := p;
                IF nexttoconvert = NIL THEN
                    nexttoconvert := p;
                END (*IF*);
                Signal (ConversionRequest);
            END (*WITH*);

        ELSE

            (* All we need to do is increment the count. *)

            INC (current^.count);

        END (*IF*);

        Release (Requests.access);

    END StartNameLookup;

(************************************************************************)

PROCEDURE CancelNameLookup (IPAddress: CARDINAL);

    (* The translation requested by StartNameLookup may be aborted. *)

    VAR Name: NameString;

    BEGIN
        TakeFromQueue (IPAddress, FALSE, Name);
    END CancelNameLookup;

(************************************************************************)

PROCEDURE GetName (IPAddress: CARDINAL;  VAR (*OUT*) Name: ARRAY OF CHAR;
                                                         Retain: BOOLEAN);

    (* Picks up the result of the query that was initiated by           *)
    (* StartNameLookup.  If Retain is TRUE we keep the result for a     *)
    (* possible future enquiry.                                         *)

    VAR answer: NameString;

    BEGIN
        TakeFromQueue (IPAddress, Retain, answer);
        Strings.Assign (answer, Name);
    END GetName;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

BEGIN
    WITH Requests DO
        head := NIL;  nexttoconvert := NIL;  tail := NIL;
        CreateLock (access);
        CreateSemaphore (ConversionRequest, 0);
    END (*WITH*);
    EVAL(CreateTask (TranslationTask, 3, "translation"));
END NameLookup.

