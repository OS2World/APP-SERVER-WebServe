(**************************************************************************)
(*                                                                        *)
(*  Web server input stream handler                                       *)
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

IMPLEMENTATION MODULE NetStream;

        (********************************************************)
        (*                                                      *)
        (*          Handler for I/O through either a TLS        *)
        (*          channel or directly through a socket        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 April 2018                    *)
        (*  Last edited:        11 November 2018                *)
        (*  Status:             Working if you don't use TLS    *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SYSTEM IMPORT ADR, CARD8, LOC;

FROM TLS IMPORT
    (* type *)  TLSsession,
    (* proc *)  OpenTLSsession, CloseTLSsession, TLSsend;

FROM TransLog IMPORT
    (* type *)  TransactionLogID;

FROM Inet2Misc IMPORT
    (* proc *)  WaitForSocketOut;

FROM Sockets IMPORT
    (* type *)  Socket,
    (* proc *)  recv, send;

FROM Watchdog IMPORT
    (* type *)  WatchdogID,
    (* proc *)  KickWatchdog;

FROM LowLevel IMPORT
    (* proc *)  Copy, EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    BufferSize = 256;
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    NStream = POINTER TO RECORD
                             TLS: BOOLEAN;
                             TLShandle: TLSsession;
                             sock: Socket;
                             bufpos, buflength: CARDINAL;
                             buffer: ARRAY [0..BufferSize-1] OF CHAR;
                         END (*RECORD*);

(************************************************************************)

PROCEDURE OpenNetStream (S: Socket;  IsServer: BOOLEAN;
                        logID: TransactionLogID;  TLS: BOOLEAN): NStream;

    (* Opens a new NStream object that handles socket data. *)

    VAR result: NStream;

    BEGIN
        NEW (result);
        WITH result^ DO
            sock := S;
            bufpos := 0;
            buflength := 0;
        END (*WITH*);
        result^.TLS := TLS;
        IF result^.TLS THEN
            result^.TLShandle := OpenTLSsession(S, IsServer, logID);
        ELSE
            result^.TLShandle := NIL;
        END (*IF*);
        RETURN result;
    END OpenNetStream;

(************************************************************************)

PROCEDURE CloseNetStream (VAR (*INOUT*) stream: NStream);

    (* Deletes the stream. *)

    BEGIN
        IF stream <> NIL THEN
            IF stream^.TLS THEN
                CloseTLSsession (stream^.TLShandle);
            END (*IF*);
            DISPOSE (stream);
        END (*IF*);
    END CloseNetStream;

(************************************************************************)

PROCEDURE GetLine (stream: NStream;  VAR (*OUT*) line: ARRAY OF CHAR): BOOLEAN;

    (* Gets one line from the specified stream.  Returns a FALSE        *)
    (* result, with no data, at end of stream.                          *)

    VAR length: CARDINAL;

    (********************************************************************)

    PROCEDURE StoreChar (ch: CHAR);

        (* Appends ch to line, except where this would cause overflow.  *)

        BEGIN
            IF length < HIGH(line) THEN
                line[length] := ch;  INC(length);
            END (*IF*);
        END StoreChar;

    (********************************************************************)

    VAR ch: CHAR;  FoundCR: BOOLEAN;

    BEGIN
        length := 0;  FoundCR := FALSE;

        LOOP
            WITH stream^ DO
                IF bufpos >= buflength THEN
                    buflength := recv (sock, buffer, BufferSize, 0);
                    IF (buflength = MAX(CARDINAL)) OR (buflength = 0) THEN
                        RETURN FALSE;
                    END (*IF*);
                    bufpos := 0;
                END (*IF*);
                ch := buffer[bufpos];  INC(bufpos);
            END (*WITH*);

            (* Our main job here is to find the line terminator.  *)

            IF FoundCR THEN
                IF ch = CR THEN
                    StoreChar(CR);
                ELSE
                    FoundCR := FALSE;
                    IF ch = LF THEN
                        StoreChar (Nul);
                        RETURN TRUE;
                    ELSE
                        StoreChar(ch);
                    END (*IF*);
                END (*IF*);
            ELSIF ch = CR THEN
                FoundCR := TRUE;
            ELSE
                IF ch = LF THEN
                    StoreChar (Nul);
                    RETURN TRUE;
                ELSE
                    StoreChar (ch);
                END (*IF*);
            END (*IF*);

        END (*LOOP*);

    END GetLine;

(************************************************************************)

PROCEDURE GetBytes (stream: NStream;  VAR (*OUT*) result: ARRAY OF CHAR;
                        wanted: CARDINAL;  VAR (*OUT*) actual: CARDINAL);

    (* Gets at most wanted chars into result,  Returns actual as the    *)
    (* number of characters found.                                      *)

    VAR pos, toget: CARDINAL;

    BEGIN
        actual := 0;  pos := 0;
        WHILE wanted > 0 DO
            WITH stream^ DO

                (* Fetch more if buffer is empty. *)

                IF bufpos >= buflength THEN
                    toget := wanted;
                    IF toget > BufferSize THEN
                        toget := BufferSize;
                    END (*IF*);
                    buflength := recv (sock, buffer, toget, 0);
                    IF (buflength = MAX(CARDINAL)) OR (buflength = 0) THEN
                        RETURN;
                    END (*IF*);
                    bufpos := 0;
                END (*IF*);

                (* Work out how much to copy to result. *)

                toget := buflength - bufpos;
                IF toget > wanted THEN
                    toget := wanted;
                END (*IF*);

                Copy (ADR(buffer[bufpos]), ADR(result[pos]), toget);
                INC (bufpos, toget);
                INC (pos, toget);
                INC (actual, toget);
                DEC (wanted, toget);

            END (*WITH*);

        END (*WHILE*);

    END GetBytes;

(************************************************************************)
(*                                  OUTPUT                              *)
(************************************************************************)

PROCEDURE PutToSocket (S: Socket;  VAR (*IN*) message: ARRAY OF LOC;
                                        length: CARDINAL): CARDINAL;

    (* Sends message to socket S.  The returned value is the number of  *)
    (* bytes sent, or MAX(CARDINAL) if there was an error.              *)

    CONST BufferSize = 65536;

    VAR pos, total, togo, xferred: CARDINAL;

    BEGIN
        pos := 0;  total := 0;  togo := length;
        WHILE togo > 0 DO
            IF WaitForSocketOut (S, MAX(CARDINAL)) > 0 THEN
                xferred := send (S, message[pos], togo, 0);
            ELSE
                xferred := MAX(CARDINAL);
            END (*IF*);
            IF xferred = MAX(CARDINAL) THEN
                total := MAX(CARDINAL);
                togo := 0;
            ELSE
                INC (total, xferred);  DEC (togo, xferred);
                INC (pos, xferred);
            END (*IF*);
        END (*WHILE*);
        RETURN total;
    END PutToSocket;

(************************************************************************)

PROCEDURE PutBytes (stream: NStream;  VAR (*IN*) message: ARRAY OF LOC;
                                                length: CARDINAL): CARDINAL;

    (* Sends a message of "length" bytes.  The returned value is the number *)
    (* of bytes sent, or MAX(CARDINAL) if there was an error.               *)

    BEGIN
        IF stream^.TLS THEN
            RETURN TLSsend (stream^.TLShandle, message, length);
        ELSE
            RETURN PutToSocket (stream^.sock, message, length);
        END (*IF*);
    END PutBytes;

(************************************************************************)

PROCEDURE PutEOL (stream: NStream);

    (* Sends CRLF.  *)

    TYPE TwoByte = ARRAY [0..1] OF CHAR;
    CONST EOL = TwoByte {CR, LF};
    VAR CRLF: TwoByte;

    BEGIN
        CRLF := EOL;
        EVAL (PutBytes (stream, CRLF, 2));
    END PutEOL;

(************************************************************************)

END NetStream.

